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
defaultEmptyBonus = 0.5

standardMaxDepth :: (Integral i) => i -> i
standardMaxDepth n = round (8 + 1.3 * (fromIntegral n))

getMinSplit :: (RealFloat f, Integral i) => f -> f -> f -> (f -> V3 f) -> i -> Shape f -> f -> [(f, i)] -> (V3 f, f, Shape f, Shape f)
getMinSplit ti tt emptyBonus splitVectorFunction numObjects aabb aabbSurfaceArea splitsAndIndices =
    let bonusFunction = (\index -> if index == 0 || index == numObjects
                                   then emptyBonus
                                   else 0)
    in foldr (\(split, index) (minSplit, minSplitCost, minLeftAABB, minRightAABB) ->
                 let (leftAABB, rightAABB) = splitBoundingBox (splitVectorFunction split) aabb
                     splitCost = tt + ti * (1 - (bonusFunction index)) * (((fromIntegral index) * (boundingBoxSurfaceArea leftAABB) / aabbSurfaceArea) +
                                                                         ((fromIntegral (numObjects - index)) * (boundingBoxSurfaceArea rightAABB) / aabbSurfaceArea))
                 in if splitCost < minSplitCost
                    then (splitVectorFunction split, splitCost, leftAABB, rightAABB)
                    else (minSplit, minSplitCost, minLeftAABB, minRightAABB)) (V3 infinity infinity infinity, (fromIntegral numObjects) * ti, aabb, aabb) splitsAndIndices

splitObjects :: (RealFloat f) => [Object f] -> [Object f] -> V3 f -> [Object f] -> ([Object f], [Object f])
splitObjects leftObjects rightObjects _ [] = (leftObjects, rightObjects)
splitObjects leftObjects rightObjects (V3 splitX splitY splitZ) (object@(Object shape _ _) : objects)
    | splitX /= infinity = let AABB frame (V3 minX _ _) (V3 maxX _ _) = getShapeBoundingBox shape
                               newLeftObjects = if minX < splitX || maxX < splitX then object : leftObjects else leftObjects
                               newRightObjects = if minX > splitX || maxX > splitX then object : rightObjects else rightObjects
                           in splitObjects newLeftObjects newRightObjects (V3 splitX infinity infinity) objects
    | splitY /= infinity = let AABB frame (V3 _ minY _) (V3 _ maxY _) = getShapeBoundingBox shape
                               newLeftObjects = if minY < splitY || maxY < splitY then object : leftObjects else leftObjects
                               newRightObjects = if minY > splitY || maxY > splitY then object : rightObjects else rightObjects
                           in splitObjects newLeftObjects newRightObjects (V3 infinity splitY infinity) objects
    | splitZ /= infinity = let AABB frame (V3 _ _ minZ) (V3 _ _ maxZ) = getShapeBoundingBox shape
                               newLeftObjects = if minZ < splitZ || maxZ < splitZ then object : leftObjects else leftObjects
                               newRightObjects = if minZ > splitZ || maxZ > splitZ then object : rightObjects else rightObjects
                           in splitObjects newLeftObjects newRightObjects (V3 infinity infinity splitZ) objects

splitNode :: (Ord f, RealFloat f, Integral i) => i -> i -> f -> f -> f -> Shape f -> [Object f] -> KDNode f
splitNode depth maxDepth ti tt emptyBonus aabb@(AABB _ minBound maxBound) objects =
    if depth > maxDepth
    then KDLeaf objects
    else let numObjects = length objects
             shapeBoundingBoxes = fmap (\(Object shape _ _) -> getShapeBoundingBox shape) objects
             aabbSurfaceArea = boundingBoxSurfaceArea aabb
             splitIndices = reverse (foldr (\index accumulator -> (index + 1) : index : accumulator) [] [0..((numObjects) - 1)])
             (V3 dx dy dz) = maxBound ^-^ minBound
             getXSplits = foldr (\(AABB _ (V3 x0 _ _) (V3 x1 _ _)) accumulator -> x1 : x0 : accumulator) []
             getYSplits = foldr (\(AABB _ (V3 _ y0 _) (V3 _ y1 _)) accumulator -> y1 : y0 : accumulator) []
             getZSplits = foldr (\(AABB _ (V3 _ _ z0) (V3 _ _ z1)) accumulator -> z1 : z0 : accumulator) []
             getXSplitVector = (\split -> V3 split infinity infinity)
             getYSplitVector = (\split -> V3 infinity split infinity)
             getZSplitVector = (\split -> V3 infinity infinity split)
         in if dx > dy
            then if dz > dx
                 then let splits = sort (getZSplits shapeBoundingBoxes)
                          splitsAndIndices = zip splits splitIndices
                          (minSplit, minSplitCost, minLeftAABB, minRightAABB) = getMinSplit ti tt emptyBonus getZSplitVector numObjects aabb aabbSurfaceArea splitsAndIndices
                      in if minSplit == (V3 infinity infinity infinity)
                         then KDLeaf objects
                         else let (leftObjects, rightObjects) = splitObjects [] [] minSplit objects
                              in KDBranch minSplit (splitNode (depth + 1) maxDepth ti tt emptyBonus minLeftAABB leftObjects) (splitNode (depth + 1) maxDepth ti tt emptyBonus minRightAABB rightObjects)
                 else let splits = sort (getXSplits shapeBoundingBoxes)
                          splitsAndIndices = zip splits splitIndices
                          (minSplit, minSplitCost, minLeftAABB, minRightAABB) = getMinSplit ti tt emptyBonus getXSplitVector numObjects aabb aabbSurfaceArea splitsAndIndices
                      in if minSplit == (V3 infinity infinity infinity)
                         then KDLeaf objects
                         else let (leftObjects, rightObjects) = splitObjects [] [] minSplit objects
                              in KDBranch minSplit (splitNode (depth + 1) maxDepth ti tt emptyBonus minLeftAABB leftObjects) (splitNode (depth + 1) maxDepth ti tt emptyBonus minRightAABB rightObjects)
            else if dz > dy
                 then let splits = sort (getZSplits shapeBoundingBoxes)
                          splitsAndIndices = zip splits splitIndices
                          (minSplit, minSplitCost, minLeftAABB, minRightAABB) = getMinSplit ti tt emptyBonus getZSplitVector numObjects aabb aabbSurfaceArea splitsAndIndices
                      in if minSplit == (V3 infinity infinity infinity)
                         then KDLeaf objects
                         else let (leftObjects, rightObjects) = splitObjects [] [] minSplit objects
                              in KDBranch minSplit (splitNode (depth + 1) maxDepth ti tt emptyBonus minLeftAABB leftObjects) (splitNode (depth + 1) maxDepth ti tt emptyBonus minRightAABB rightObjects)
                 else let splits = sort (getYSplits shapeBoundingBoxes)
                          splitsAndIndices = zip splits splitIndices
                          (minSplit, minSplitCost, minLeftAABB, minRightAABB) = getMinSplit ti tt emptyBonus getYSplitVector numObjects aabb aabbSurfaceArea splitsAndIndices
                      in if minSplit == (V3 infinity infinity infinity)
                         then KDLeaf objects
                         else let (leftObjects, rightObjects) = splitObjects [] [] minSplit objects
                              in KDBranch minSplit (splitNode (depth + 1) maxDepth ti tt emptyBonus minLeftAABB leftObjects) (splitNode (depth + 1) maxDepth ti tt emptyBonus minRightAABB rightObjects)

buildKDTree :: (RealFloat f, Integral i) => f -> f -> f -> (i -> i) -> [Object f] -> KDTree f
buildKDTree ti tt emptyBonus maxDepthFunction [] = KDTree (AABB identity (V3 infinity infinity infinity) (V3 (-infinity) (-infinity) (-infinity))) (KDLeaf [])
buildKDTree ti tt emptyBonus maxDepthFunction objects =
    let maxDepth = maxDepthFunction (fromIntegral (length objects))
        treeAABB = foldr (\aabb accumulatorAABB -> 
                            case mergeBoundingBoxes aabb accumulatorAABB of
                                Nothing -> accumulatorAABB
                                Just mergedAABB -> mergedAABB) (AABB identity (V3 infinity infinity infinity) (V3 (-infinity) (-infinity) (-infinity))) (fmap (\(Object shape _ _) -> getShapeBoundingBox shape) objects)
    in KDTree treeAABB (splitNode 0 maxDepth ti tt emptyBonus treeAABB objects)

