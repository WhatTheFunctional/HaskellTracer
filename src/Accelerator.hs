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

data KDNode f = KDLeaf [Shape f] -- Leaf shapes
              | KDBranch (V3 f) (KDNode f) (KDNode f) -- Split and child-nodes
              deriving (Show, Eq)

data KDTree f = KDTree (Shape f) (KDNode f) -- AABB and root node
              deriving (Show, Eq)

defaultTi :: (Num f) => f
defaultTi = 80

defaultTt :: (Num f) => f
defaultTt = 1

defaultEmptyBonus :: (Fractional f) => f
defaultEmptyBonus = 0.5

standardMaxDepth :: (Integral i) => i -> i
standardMaxDepth n = round (8 + 1.3 * (fromIntegral n))

getMinSplit :: (RealFloat f, Integral i) => f -> f -> f -> (f -> V3 f) -> i -> Shape f -> f -> [(f, i)] -> (V3 f, f, Shape f, Shape f)
getMinSplit ti tt emptyBonus splitVectorFunction numShapes aabb aabbSurfaceArea splitsAndIndices =
    let bonusFunction = (\index -> if index == 0 || index == numShapes
                                   then emptyBonus
                                   else 0)
    in foldr (\(split, index) (minSplit, minSplitCost, minLeftAABB, minRightAABB) ->
                 let (leftAABB, rightAABB) = splitBoundingBox (splitVectorFunction split) aabb
                     splitCost = tt + ti * (1 - (bonusFunction index)) * (((fromIntegral index) * (boundingBoxSurfaceArea leftAABB) / aabbSurfaceArea) +
                                                                         ((fromIntegral (numShapes - index)) * (boundingBoxSurfaceArea rightAABB) / aabbSurfaceArea))
                 in if splitCost < minSplitCost
                    then (splitVectorFunction split, splitCost, leftAABB, rightAABB)
                    else (minSplit, minSplitCost, minLeftAABB, minRightAABB)) (V3 infinity infinity infinity, (fromIntegral numShapes) * ti, aabb, aabb) splitsAndIndices

splitShapes :: (RealFloat f) => [Shape f] -> [Shape f] -> V3 f -> [Shape f] -> ([Shape f], [Shape f])
splitShapes leftShapes rightShapes _ [] = (leftShapes, rightShapes)
splitShapes leftShapes rightShapes (V3 splitX splitY splitZ) (shape : shapes)
    | splitX /= infinity = let AABB frame (V3 minX _ _) (V3 maxX _ _) = getShapeBoundingBox shape
                               newLeftShapes = if minX < splitX || maxX < splitX then shape : leftShapes else leftShapes
                               newRightShapes = if minX > splitX || maxX > splitX then shape : rightShapes else rightShapes
                           in splitShapes newLeftShapes newRightShapes (V3 splitX infinity infinity) shapes
    | splitY /= infinity = let AABB frame (V3 _ minY _) (V3 _ maxY _) = getShapeBoundingBox shape
                               newLeftShapes = if minY < splitY || maxY < splitY then shape : leftShapes else leftShapes
                               newRightShapes = if minY > splitY || maxY > splitY then shape : rightShapes else rightShapes
                           in splitShapes newLeftShapes newRightShapes (V3 infinity splitY infinity) shapes
    | splitZ /= infinity = let AABB frame (V3 _ _ minZ) (V3 _ _ maxZ) = getShapeBoundingBox shape
                               newLeftShapes = if minZ < splitZ || maxZ < splitZ then shape : leftShapes else leftShapes
                               newRightShapes = if minZ > splitZ || maxZ > splitZ then shape : rightShapes else rightShapes
                           in splitShapes newLeftShapes newRightShapes (V3 infinity infinity splitZ) shapes

splitNode :: (Ord f, RealFloat f, Integral i) => i -> i -> f -> f -> f -> Shape f -> [Shape f] -> KDNode f
splitNode depth maxDepth ti tt emptyBonus aabb@(AABB _ minBound maxBound) shapes =
    if depth > maxDepth
    then KDLeaf shapes
    else let numShapes = length shapes
             shapeBoundingBoxes = fmap getShapeBoundingBox shapes
             aabbSurfaceArea = boundingBoxSurfaceArea aabb
             splitIndices = reverse (foldr (\index accumulator -> (index + 1) : index : accumulator) [] [0..((numShapes) - 1)])
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
                          (minSplit, minSplitCost, minLeftAABB, minRightAABB) = getMinSplit ti tt emptyBonus getZSplitVector numShapes aabb aabbSurfaceArea splitsAndIndices
                      in if minSplit == (V3 infinity infinity infinity)
                         then KDLeaf shapes
                         else let (leftShapes, rightShapes) = splitShapes [] [] minSplit shapes
                              in KDBranch minSplit (splitNode (depth + 1) maxDepth ti tt emptyBonus minLeftAABB leftShapes) (splitNode (depth + 1) maxDepth ti tt emptyBonus minRightAABB rightShapes)
                 else let splits = sort (getXSplits shapeBoundingBoxes)
                          splitsAndIndices = zip splits splitIndices
                          (minSplit, minSplitCost, minLeftAABB, minRightAABB) = getMinSplit ti tt emptyBonus getXSplitVector numShapes aabb aabbSurfaceArea splitsAndIndices
                      in if minSplit == (V3 infinity infinity infinity)
                         then KDLeaf shapes
                         else let (leftShapes, rightShapes) = splitShapes [] [] minSplit shapes
                              in KDBranch minSplit (splitNode (depth + 1) maxDepth ti tt emptyBonus minLeftAABB leftShapes) (splitNode (depth + 1) maxDepth ti tt emptyBonus minRightAABB rightShapes)
            else if dz > dy
                 then let splits = sort (getZSplits shapeBoundingBoxes)
                          splitsAndIndices = zip splits splitIndices
                          (minSplit, minSplitCost, minLeftAABB, minRightAABB) = getMinSplit ti tt emptyBonus getZSplitVector numShapes aabb aabbSurfaceArea splitsAndIndices
                      in if minSplit == (V3 infinity infinity infinity)
                         then KDLeaf shapes
                         else let (leftShapes, rightShapes) = splitShapes [] [] minSplit shapes
                              in KDBranch minSplit (splitNode (depth + 1) maxDepth ti tt emptyBonus minLeftAABB leftShapes) (splitNode (depth + 1) maxDepth ti tt emptyBonus minRightAABB rightShapes)
                 else let splits = sort (getYSplits shapeBoundingBoxes)
                          splitsAndIndices = zip splits splitIndices
                          (minSplit, minSplitCost, minLeftAABB, minRightAABB) = getMinSplit ti tt emptyBonus getYSplitVector numShapes aabb aabbSurfaceArea splitsAndIndices
                      in if minSplit == (V3 infinity infinity infinity)
                         then KDLeaf shapes
                         else let (leftShapes, rightShapes) = splitShapes [] [] minSplit shapes
                              in KDBranch minSplit (splitNode (depth + 1) maxDepth ti tt emptyBonus minLeftAABB leftShapes) (splitNode (depth + 1) maxDepth ti tt emptyBonus minRightAABB rightShapes)

buildKDTree :: (RealFloat f, Integral i) => f -> f -> f -> (i -> i) -> [Shape f] -> KDTree f
buildKDTree ti tt emptyBonus maxDepthFunction [] = KDTree (AABB identity (V3 infinity infinity infinity) (V3 (-infinity) (-infinity) (-infinity))) (KDLeaf [])
buildKDTree ti tt emptyBonus maxDepthFunction shapes =
    let maxDepth = maxDepthFunction (fromIntegral (length shapes))
        treeAABB = foldr (\aabb accumulatorAABB -> 
                            case mergeBoundingBoxes aabb accumulatorAABB of
                                Nothing -> accumulatorAABB
                                Just mergedAABB -> mergedAABB) (AABB identity (V3 infinity infinity infinity) (V3 (-infinity) (-infinity) (-infinity))) (fmap getShapeBoundingBox shapes)
    in KDTree treeAABB (splitNode 0 maxDepth ti tt emptyBonus treeAABB shapes)

