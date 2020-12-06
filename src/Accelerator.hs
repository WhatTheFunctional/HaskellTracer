module Accelerator
    ( KDNode (..)
    , KDTree (..)
    , defaultTi
    , defaultTt
    , defaultEmptyBonus
    , standardMaxDepth
    , getNodeBoundingBox 
    , mergeObjectBoundingBoxes 
    , buildKDTree
    , toMesh
    , treeToMesh
    , treeToLeafMesh
    ) where

import Data.List
import Control.Parallel
import Numeric.Limits
import Linear
import Linear.Affine

import Geometry
import Object

data KDNode = KDLeaf Shape [Object] -- Leaf objects
              | KDBranch (V3 Double) Shape (KDNode) (KDNode) -- Split, AABB and child-nodes
              deriving (Show)

data KDTree = KDTree (Shape) [Object] (KDNode) -- AABB, planes and root node
              deriving (Show)

data IntervalEdge = LeftEdge | RightEdge deriving (Show) -- Label for the edge of an interval


defaultTi :: Double
defaultTi = 80

defaultTt :: Double
defaultTt = 1

defaultEmptyBonus :: Double
defaultEmptyBonus = 0.01

standardMaxDepth :: Int -> Int
standardMaxDepth n = round (8 + 1.3 * (logBase 2 (fromIntegral n)))

getNodeBoundingBox :: KDNode -> Shape
getNodeBoundingBox (KDLeaf aabb _) = aabb
getNodeBoundingBox (KDBranch _ aabb _ _) = aabb

mergeObjectBoundingBoxes :: [Object] -> Shape
mergeObjectBoundingBoxes nonPlanes =
    foldl' (\accumulatorAABB aabb -> 
       case mergeBoundingBoxes aabb accumulatorAABB of
           Nothing -> accumulatorAABB
           Just mergedAABB -> mergedAABB) infinityAABB (fmap (\(Object shape _ _) -> getShapeBoundingBox shape) nonPlanes)


getMinSplit :: Double -> Double -> Double -> Int -> Shape -> Double -> [(V3 Double, (Int, Int))] -> (V3 Double, Double)
getMinSplit ti tt emptyBonus numObjects aabb aabbSurfaceArea splitsAndIndices =
    let bonusFunction = (\count -> if count == 0 || count == numObjects
                                   then emptyBonus -- One AABB is empty
                                   else 0)
        invAABBSurfaceArea = 1 / aabbSurfaceArea
        noSplitCost = ti * (fromIntegral numObjects)
    in foldl' (\(minSplit, minSplitCost) (split, (leftCount, rightCount)) ->
                 let (leftAABB, rightAABB) = splitBoundingBox split aabb
                     bonusFactor = (1 - (bonusFunction leftCount) - (bonusFunction rightCount))
                     leftFactor = (fromIntegral leftCount) * (boundingBoxSurfaceArea leftAABB) * invAABBSurfaceArea
                     rightFactor = (fromIntegral rightCount) * (boundingBoxSurfaceArea rightAABB) * invAABBSurfaceArea
                     splitCost = tt + ti * bonusFactor * (leftFactor + rightFactor)
                 in if splitCost < minSplitCost
                    then (split, splitCost)
                    else (minSplit, minSplitCost)) (V3 infinity infinity infinity, noSplitCost) splitsAndIndices

splitObjects :: (V3 Double -> Double) -> [Object] -> [Object] -> V3 Double -> [Object] -> ([Object], [Object])
splitObjects _ leftObjects rightObjects _ [] = (leftObjects, rightObjects)
splitObjects getCoord leftObjects rightObjects splitV (object@(Object shape _ _) : objects) =
    let split = getCoord splitV
        AABB _ minV maxV = getShapeBoundingBox shape
        minB = getCoord minV
        maxB = getCoord maxV
        newLeftObjects = if maxB <= split || (minB <= split && maxB >= split) then object : leftObjects else leftObjects
        newRightObjects = if minB >= split || (minB <= split && maxB >= split) then object : rightObjects else rightObjects
    in splitObjects getCoord newLeftObjects newRightObjects splitV objects

computeSplitIndices :: (Int, Int) -> [(Double, IntervalEdge)] -> [(Int, Int)] -> [(Int, Int)]
computeSplitIndices _ [] counts = counts
computeSplitIndices (leftCount, rightCount) ((_, LeftEdge) : splits) counts = computeSplitIndices (leftCount + 1, rightCount) splits ((leftCount + 1, rightCount) : counts)
computeSplitIndices (leftCount, rightCount) ((_, RightEdge) : splits) counts = computeSplitIndices (leftCount, rightCount - 1) splits ((leftCount, rightCount - 1) : counts)

getSplits :: (V3 Double -> Double) -> [Shape] -> [(Double, IntervalEdge)]
getSplits getCoord objects = foldl' (\accumulator (AABB _ v0 v1) -> (getCoord v0, LeftEdge) : (getCoord v1, RightEdge) : accumulator) [] objects

splitBestAxis :: (V3 Double -> Double) -> ((Double, IntervalEdge) -> V3 Double) -> Int -> Int -> Int -> Double -> Double -> Double -> Shape -> [Object] -> KDNode
splitBestAxis getCoord getSplitVector numObjects currentDepth maxDepth ti tt emptyBonus aabb objects =
    let shapeBoundingBoxes = fmap (\(Object shape _ _) -> getShapeBoundingBox shape) objects
        splits = sortBy (\(lhs, lhsEdge) (rhs, rhsEdge) -> compare lhs rhs) (getSplits getCoord shapeBoundingBoxes) -- Sort splits by coordinate
        splitIndices = reverse $ computeSplitIndices (0, numObjects) splits []
        splitsAndIndices = zip (fmap getSplitVector splits) splitIndices
        aabbSurfaceArea = boundingBoxSurfaceArea aabb
        (minSplit, minSplitCost) = getMinSplit ti tt emptyBonus numObjects aabb aabbSurfaceArea splitsAndIndices
    in if minSplit == (V3 infinity infinity infinity)
       then KDLeaf (mergeObjectBoundingBoxes objects) objects
       else let (leftObjects, rightObjects) = splitObjects getCoord [] [] minSplit objects
                minLeftAABB = mergeObjectBoundingBoxes leftObjects
                minRightAABB = mergeObjectBoundingBoxes rightObjects
                leftNode = splitNode (currentDepth + 1) maxDepth ti tt emptyBonus minLeftAABB leftObjects
                rightNode = splitNode (currentDepth + 1) maxDepth ti tt emptyBonus minRightAABB rightObjects
            in KDBranch minSplit aabb leftNode rightNode

splitNode :: Int -> Int -> Double -> Double -> Double -> Shape -> [Object] -> KDNode
splitNode depth maxDepth ti tt emptyBonus aabb@(AABB _ minBound maxBound) objects =
    let numObjects = fromIntegral (length objects)
    in if depth > maxDepth || numObjects <= 16
       then KDLeaf (mergeObjectBoundingBoxes objects) objects
       else let getX = (\(V3 x _ _) -> x)
                getY = (\(V3 _ y _) -> y)
                getZ = (\(V3 _ _ z) -> z)
                getXSplitVector = (\(split, _) -> V3 split infinity infinity)
                getYSplitVector = (\(split, _) -> V3 infinity split infinity)
                getZSplitVector = (\(split, _) -> V3 infinity infinity split)
                (V3 dx dy dz) = maxBound ^-^ minBound
            in if dx > dy
               then if dz > dx
                    then splitBestAxis getZ getZSplitVector numObjects depth maxDepth ti tt emptyBonus aabb objects
                    else splitBestAxis getX getXSplitVector numObjects depth maxDepth ti tt emptyBonus aabb objects
               else if dz > dy
                    then splitBestAxis getZ getZSplitVector numObjects depth maxDepth ti tt emptyBonus aabb objects
                    else splitBestAxis getY getYSplitVector numObjects depth maxDepth ti tt emptyBonus aabb objects

filterOutPlanes :: ([Object], [Object])
            -> [Object]
            -> ([Object], [Object])
filterOutPlanes (planes, nonPlanes) [] = (planes, nonPlanes)
filterOutPlanes (planes, nonPlanes) (object@(Object (Plane _ _) _ _) : objects) = filterOutPlanes (object : planes, nonPlanes) objects
filterOutPlanes (planes, nonPlanes) (object : objects) = filterOutPlanes (planes, object : nonPlanes) objects

buildKDTree :: Double -> Double -> Double -> (Int -> Int) -> [Object] -> KDTree
buildKDTree ti tt emptyBonus maxDepthFunction objects =
    let (planes, nonPlanes) = filterOutPlanes ([], []) objects
        maxDepth = maxDepthFunction (fromIntegral (length nonPlanes))
        treeAABB = mergeObjectBoundingBoxes nonPlanes
        rootNode = (splitNode (fromIntegral 0) maxDepth ti tt emptyBonus treeAABB nonPlanes)
    in KDTree treeAABB planes rootNode

toMesh :: KDNode -> (Int, [V3 Double], [Int]) -> (Int, [V3 Double], [Int])
toMesh (KDLeaf _ objects) (currentIndex, vertices, indices) = (currentIndex, vertices, indices)
toMesh (KDBranch split _ left right) (currentIndex, vertices, indices) =
    let lAABB@(AABB leftFrame (V3 lminX lminY lminZ) (V3 lmaxX lmaxY lmaxZ)) = getNodeBoundingBox left
        rAABB@(AABB rightFrame (V3 rminX rminY rminZ) (V3 rmaxX rmaxY rmaxZ)) = getNodeBoundingBox right
        (leftIndex, leftVertices, leftIndices) = toMesh left
            (
                currentIndex + 8,
                (V3 lminX lminY lminZ) :
                (V3 lmaxX lminY lminZ) :
                (V3 lminX lmaxY lminZ) :
                (V3 lmaxX lmaxY lminZ) :
                (V3 lminX lminY lmaxZ) :
                (V3 lmaxX lminY lmaxZ) :
                (V3 lminX lmaxY lmaxZ) :
                (V3 lmaxX lmaxY lmaxZ) : vertices,
                (currentIndex + 0) : (currentIndex + 1) :
                (currentIndex + 2) : (currentIndex + 3) :
                (currentIndex + 4) : (currentIndex + 5) :
                (currentIndex + 6) : (currentIndex + 7) :
                (currentIndex + 0) : (currentIndex + 2) :
                (currentIndex + 1) : (currentIndex + 3) :
                (currentIndex + 4) : (currentIndex + 6) :
                (currentIndex + 5) : (currentIndex + 7) :
                (currentIndex + 0) : (currentIndex + 4) :
                (currentIndex + 1) : (currentIndex + 5) :
                (currentIndex + 2) : (currentIndex + 6) :
                (currentIndex + 3) : (currentIndex + 7) : indices)
        (rightIndex, rightVertices, rightIndices) = toMesh right
            (
                leftIndex + 8,
                (V3 rminX rminY rminZ) :
                (V3 rmaxX rminY rminZ) :
                (V3 rminX rmaxY rminZ) :
                (V3 rmaxX rmaxY rminZ) :
                (V3 rminX rminY rmaxZ) :
                (V3 rmaxX rminY rmaxZ) :
                (V3 rminX rmaxY rmaxZ) :
                (V3 rmaxX rmaxY rmaxZ) : leftVertices,
                (leftIndex + 0) : (leftIndex + 1) :
                (leftIndex + 2) : (leftIndex + 3) :
                (leftIndex + 4) : (leftIndex + 5) :
                (leftIndex + 6) : (leftIndex + 7) :
                (leftIndex + 0) : (leftIndex + 2) :
                (leftIndex + 1) : (leftIndex + 3) :
                (leftIndex + 4) : (leftIndex + 6) :
                (leftIndex + 5) : (leftIndex + 7) :
                (leftIndex + 0) : (leftIndex + 4) :
                (leftIndex + 1) : (leftIndex + 5) :
                (leftIndex + 2) : (leftIndex + 6) :
                (leftIndex + 3) : (leftIndex + 7) : leftIndices)
    in (rightIndex, rightVertices, rightIndices)

toLeafMesh :: KDNode -> (Int, [V3 Double], [Int]) -> (Int, [V3 Double], [Int])
toLeafMesh (KDLeaf (AABB frame (V3 minX minY minZ) (V3 maxX maxY maxZ)) objects) (currentIndex, vertices, indices) =
    (
        currentIndex + 8,
        (V3 minX minY minZ) :
        (V3 maxX minY minZ) :
        (V3 minX maxY minZ) :
        (V3 maxX maxY minZ) :
        (V3 minX minY maxZ) :
        (V3 maxX minY maxZ) :
        (V3 minX maxY maxZ) :
        (V3 maxX maxY maxZ) : vertices,
        (currentIndex + 0) : (currentIndex + 1) :
        (currentIndex + 2) : (currentIndex + 3) :
        (currentIndex + 4) : (currentIndex + 5) :
        (currentIndex + 6) : (currentIndex + 7) :
        (currentIndex + 0) : (currentIndex + 2) :
        (currentIndex + 1) : (currentIndex + 3) :
        (currentIndex + 4) : (currentIndex + 6) :
        (currentIndex + 5) : (currentIndex + 7) :
        (currentIndex + 0) : (currentIndex + 4) :
        (currentIndex + 1) : (currentIndex + 5) :
        (currentIndex + 2) : (currentIndex + 6) :
        (currentIndex + 3) : (currentIndex + 7) : indices)
toLeafMesh (KDBranch split aabb left right) (currentIndex, vertices, indices) =
    let (lAABB@(AABB leftFrame (V3 lminX lminY lminZ) (V3 lmaxX lmaxY lmaxZ)), rAABB@(AABB rightFrame (V3 rminX rminY rminZ) (V3 rmaxX rmaxY rmaxZ))) = splitBoundingBox split aabb
        (leftIndex, leftVertices, leftIndices) = toLeafMesh left (currentIndex, vertices, indices)
        (rightIndex, rightVertices, rightIndices) = toLeafMesh right (leftIndex, leftVertices, leftIndices)
    in (rightIndex, rightVertices, rightIndices)

treeToMesh :: KDTree -> ([V3 Double], [Int])
treeToMesh (KDTree aabb objects node) =
    let (currentIndex, vertices, indices) = toMesh node (0, [], [])
    in (vertices, indices)

treeToLeafMesh :: KDTree -> ([V3 Double], [Int])
treeToLeafMesh (KDTree aabb objects node) =
    let (currentIndex, vertices, indices) = toLeafMesh node (0, [], [])
    in (vertices, indices)
