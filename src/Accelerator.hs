module Accelerator
    ( KDNode (..)
    , KDTree (..)
    , defaultTi
    , defaultTt
    , defaultEmptyBonus
    , standardMaxDepth
    , buildKDTree
    ) where

import Data.List
import Numeric.Limits
import Linear
import Linear.Affine

import Geometry
import Object

data KDNode f = KDLeaf [Object f] -- Leaf objects
              | KDBranch (V3 f) (KDNode f) (KDNode f) -- Split and child-nodes
              deriving (Show)

data KDTree f = KDTree (Shape f) (KDNode f) -- AABB and root node
              deriving (Show)

defaultTi :: (Num f) => f
defaultTi = 80

defaultTt :: (Num f) => f
defaultTt = 1

defaultEmptyBonus :: (Fractional f) => f
defaultEmptyBonus = 0.01

standardMaxDepth :: (Integral i) => i -> i
standardMaxDepth n = round (8 + 1.3 * (logBase 2 (fromIntegral n)))

getMinSplit :: (RealFloat f, Integral i) => f -> f -> f -> i -> Shape f -> f -> [(V3 f, i)] -> (V3 f, f, Shape f, Shape f)
getMinSplit ti tt emptyBonus numObjects aabb aabbSurfaceArea splitsAndIndices =
    let bonusFunction = (\index -> if index == 0 || index == numObjects
                                   then emptyBonus -- One AABB is empty
                                   else 0)
        invAABBSurfaceArea = 1 / aabbSurfaceArea
        noSplitCost = ti * (fromIntegral numObjects)
    in foldr (\(split, index) (minSplit, minSplitCost, minLeftAABB, minRightAABB) ->
                 let (leftAABB, rightAABB) = splitBoundingBox split aabb
                     bonusFactor = (1 - (bonusFunction index))
                     leftFactor = (fromIntegral index) * (boundingBoxSurfaceArea leftAABB) * invAABBSurfaceArea
                     rightFactor = (fromIntegral (numObjects - index)) * (boundingBoxSurfaceArea rightAABB) * invAABBSurfaceArea
                     splitCost = tt + ti * bonusFactor * (leftFactor + rightFactor)
                 in if splitCost < minSplitCost
                    then (split, splitCost, leftAABB, rightAABB)
                    else (minSplit, minSplitCost, minLeftAABB, minRightAABB)) (V3 infinity infinity infinity, noSplitCost, aabb, aabb) splitsAndIndices

splitObjects :: (RealFloat f) => (V3 f -> f) -> [Object f] -> [Object f] -> V3 f -> [Object f] -> ([Object f], [Object f])
splitObjects _ leftObjects rightObjects _ [] = (leftObjects, rightObjects)
splitObjects getCoord leftObjects rightObjects splitV (object@(Object shape _ _) : objects) =
    let split = getCoord splitV
        AABB _ minV maxV = getShapeBoundingBox shape
        minB = getCoord minV
        maxB = getCoord maxV
        newLeftObjects = if minB <= split || maxB <= split then object : leftObjects else leftObjects
        newRightObjects = if minB >= split || maxB >= split then object : rightObjects else rightObjects
    in splitObjects getCoord newLeftObjects newRightObjects splitV objects

splitBestAxis :: (Ord f, RealFloat f, Integral i) => (V3 f -> f) -> (f -> V3 f) -> i -> i -> i -> f -> f -> f -> Shape f -> [Object f] -> KDNode f
splitBestAxis getCoord getSplitVector numObjects currentDepth maxDepth ti tt emptyBonus aabb objects =
    let shapeBoundingBoxes = fmap (\(Object shape _ _) -> getShapeBoundingBox shape) objects
        getSplits = (\objects -> foldr (\(AABB _ v0 v1) accumulator -> (getCoord v0) : (getCoord v1) : accumulator) [] objects)
        splits = fmap getSplitVector (sort (getSplits shapeBoundingBoxes))
        splitIndices = foldr (\index accumulator -> index : (index + 1) : accumulator) [] [0..(numObjects - 1)]
        splitsAndIndices = zip splits splitIndices
        aabbSurfaceArea = boundingBoxSurfaceArea aabb
        (minSplit, minSplitCost, minLeftAABB, minRightAABB) = getMinSplit ti tt emptyBonus numObjects aabb aabbSurfaceArea splitsAndIndices
    in if minSplit == (V3 infinity infinity infinity)
       then KDLeaf objects
       else let (leftObjects, rightObjects) = splitObjects getCoord [] [] minSplit objects
                leftNode = splitNode (currentDepth + 1) maxDepth ti tt emptyBonus minLeftAABB leftObjects
                rightNode = splitNode (currentDepth + 1) maxDepth ti tt emptyBonus minRightAABB rightObjects
            in KDBranch minSplit leftNode rightNode

splitNode :: (Ord f, RealFloat f, Integral i) => i -> i -> f -> f -> f -> Shape f -> [Object f] -> KDNode f
splitNode depth maxDepth ti tt emptyBonus aabb@(AABB _ minBound maxBound) objects =
    let numObjects = fromIntegral (length objects)
    in if depth > maxDepth || numObjects < 16
       then KDLeaf objects
       else let getX = (\(V3 x _ _) -> x)
                getY = (\(V3 _ y _) -> y)
                getZ = (\(V3 _ _ z) -> z)
                getXSplitVector = (\split -> V3 split infinity infinity)
                getYSplitVector = (\split -> V3 infinity split infinity)
                getZSplitVector = (\split -> V3 infinity infinity split)
                (V3 dx dy dz) = maxBound ^-^ minBound
            in if dx > dy
               then if dz > dx
                    then splitBestAxis getZ getZSplitVector numObjects depth maxDepth ti tt emptyBonus aabb objects
                    else splitBestAxis getX getXSplitVector numObjects depth maxDepth ti tt emptyBonus aabb objects
               else if dz > dy
                    then splitBestAxis getZ getZSplitVector numObjects depth maxDepth ti tt emptyBonus aabb objects
                    else splitBestAxis getY getYSplitVector numObjects depth maxDepth ti tt emptyBonus aabb objects

buildKDTree :: (RealFloat f, Integral i) => f -> f -> f -> (i -> i) -> [Object f] -> KDTree f
buildKDTree ti tt emptyBonus maxDepthFunction [] = KDTree (AABB identity (V3 infinity infinity infinity) (V3 (-infinity) (-infinity) (-infinity))) (KDLeaf [])
buildKDTree ti tt emptyBonus maxDepthFunction objects =
    let maxDepth = maxDepthFunction (fromIntegral (length objects))
        treeAABB = foldr (\aabb accumulatorAABB -> 
                              case mergeBoundingBoxes aabb accumulatorAABB of
                                  Nothing -> accumulatorAABB
                                  Just mergedAABB -> mergedAABB) (AABB identity (V3 infinity infinity infinity) (V3 (-infinity) (-infinity) (-infinity))) (fmap (\(Object shape _ _) -> getShapeBoundingBox shape) objects)
    in KDTree treeAABB (splitNode (fromIntegral 0) maxDepth ti tt emptyBonus treeAABB objects)

