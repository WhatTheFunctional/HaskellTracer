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
import Control.Parallel
import Numeric.Limits
import Linear
import Linear.Affine

import Geometry
import Object

data KDNode = KDLeaf [Object] -- Leaf objects
              | KDBranch (V3 Double) (KDNode) (KDNode) -- Split and child-nodes
              deriving (Show)

data KDTree = KDTree (Shape) [Object] (KDNode) -- AABB, planes and root node
              deriving (Show)

defaultTi :: Double
defaultTi = 80

defaultTt :: Double
defaultTt = 1

defaultEmptyBonus :: Double
defaultEmptyBonus = 0.01

standardMaxDepth :: Int -> Int
standardMaxDepth n = round (8 + 1.3 * (logBase 2 (fromIntegral n)))

getMinSplit :: Double -> Double -> Double -> Int -> Shape -> Double -> [(V3 Double, Int)] -> (V3 Double, Double, Shape, Shape)
getMinSplit ti tt emptyBonus numObjects aabb aabbSurfaceArea splitsAndIndices =
    let bonusFunction = (\index -> if index == 0 || index == numObjects
                                   then emptyBonus -- One AABB is empty
                                   else 0)
        invAABBSurfaceArea = 1 / aabbSurfaceArea
        noSplitCost = ti * (fromIntegral numObjects)
    in foldl' (\(minSplit, minSplitCost, minLeftAABB, minRightAABB) (split, index) ->
                 let (leftAABB, rightAABB) = splitBoundingBox split aabb
                     bonusFactor = (1 - (bonusFunction index))
                     leftFactor = (fromIntegral index) * (boundingBoxSurfaceArea leftAABB) * invAABBSurfaceArea
                     rightFactor = (fromIntegral (numObjects - index)) * (boundingBoxSurfaceArea rightAABB) * invAABBSurfaceArea
                     splitCost = tt + ti * bonusFactor * (leftFactor + rightFactor)
                 in if splitCost < minSplitCost
                    then (split, splitCost, leftAABB, rightAABB)
                    else (minSplit, minSplitCost, minLeftAABB, minRightAABB)) (V3 infinity infinity infinity, noSplitCost, aabb, aabb) splitsAndIndices

splitObjects :: (V3 Double -> Double) -> [Object] -> [Object] -> V3 Double -> [Object] -> ([Object], [Object])
splitObjects _ leftObjects rightObjects _ [] = (leftObjects, rightObjects)
splitObjects getCoord leftObjects rightObjects splitV (object@(Object shape _ _) : objects) =
    let split = getCoord splitV
        AABB _ minV maxV = getShapeBoundingBox shape
        minB = getCoord minV
        maxB = getCoord maxV
        newLeftObjects = if minB <= split || maxB <= split then object : leftObjects else leftObjects
        newRightObjects = if minB >= split || maxB >= split then object : rightObjects else rightObjects
    in splitObjects getCoord newLeftObjects newRightObjects splitV objects

splitBestAxis :: (V3 Double -> Double) -> (Double -> V3 Double) -> Int -> Int -> Int -> Double -> Double -> Double -> Shape -> [Object] -> KDNode
splitBestAxis getCoord getSplitVector numObjects currentDepth maxDepth ti tt emptyBonus aabb objects =
    let shapeBoundingBoxes = fmap (\(Object shape _ _) -> getShapeBoundingBox shape) objects
        getSplits = (\objects -> foldl' (\accumulator (AABB _ v0 v1) -> (getCoord v0) : (getCoord v1) : accumulator) [] objects)
        splits = fmap getSplitVector (sort (getSplits shapeBoundingBoxes))
        splitIndices = foldl' (\accumulator index -> index : (index + 1) : accumulator) [] [0..(numObjects - 1)]
        splitsAndIndices = zip splits splitIndices
        aabbSurfaceArea = boundingBoxSurfaceArea aabb
        (minSplit, minSplitCost, minLeftAABB, minRightAABB) = getMinSplit ti tt emptyBonus numObjects aabb aabbSurfaceArea splitsAndIndices
    in if minSplit == (V3 infinity infinity infinity)
       then KDLeaf objects
       else let (leftObjects, rightObjects) = splitObjects getCoord [] [] minSplit objects
                leftNode = splitNode (currentDepth + 1) maxDepth ti tt emptyBonus minLeftAABB leftObjects
                rightNode = splitNode (currentDepth + 1) maxDepth ti tt emptyBonus minRightAABB rightObjects
            in KDBranch minSplit leftNode rightNode

splitNode :: Int -> Int -> Double -> Double -> Double -> Shape -> [Object] -> KDNode
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

splitPlanes :: ([Object], [Object])
            -> [Object]
            -> ([Object], [Object])
splitPlanes (planes, nonPlanes) [] = (planes, nonPlanes)
splitPlanes (planes, nonPlanes) (object@(Object (Plane _ _) _ _) : objects) = splitPlanes (object : planes, nonPlanes) objects
splitPlanes (planes, nonPlanes) (object : objects) = splitPlanes (planes, object : nonPlanes) objects

buildKDTree :: Double -> Double -> Double -> (Int -> Int) -> [Object] -> KDTree
buildKDTree ti tt emptyBonus maxDepthFunction [] = KDTree (AABB identity (V3 infinity infinity infinity) (V3 (-infinity) (-infinity) (-infinity))) [] (KDLeaf [])
buildKDTree ti tt emptyBonus maxDepthFunction objects =
    let (planes, nonPlanes) = splitPlanes ([], []) objects
        maxDepth = maxDepthFunction (fromIntegral (length nonPlanes))
        treeAABB = foldl' (\accumulatorAABB aabb -> 
                               case mergeBoundingBoxes aabb accumulatorAABB of
                                   Nothing -> accumulatorAABB
                                   Just mergedAABB -> mergedAABB) (AABB identity (V3 infinity infinity infinity) (V3 (-infinity) (-infinity) (-infinity))) (fmap (\(Object shape _ _) -> getShapeBoundingBox shape) nonPlanes)
    in KDTree treeAABB planes (splitNode (fromIntegral 0) maxDepth ti tt emptyBonus treeAABB nonPlanes)

