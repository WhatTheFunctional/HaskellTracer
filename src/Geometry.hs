module Geometry
    ( Shape (..)
    , infinityAABB
    , transformShape
    , getShapeBoundingBox
    , mergeBoundingBoxes
    , splitBoundingBox
    , boundingBoxSurfaceArea
    ) where

import Numeric.Limits
import Linear
import Linear.Affine
import Linear.Matrix

data Shape = Plane (Point V3 Double) (V3 Double) -- Point and normal
           | Sphere (Point V3 Double) Double -- Center and radius
           | AABB (M44 Double) (V3 Double) (V3 Double) -- Frame, min, and max
           | Triangle (Point V3 Double) (Point V3 Double) (Point V3 Double) (V3 Double) -- Points, normal
           | Disk (Point V3 Double) (V3 Double) Double -- Point, normal, and radius
           | Rectangle (Point V3 Double) (V3 Double) (V3 Double) (V3 Double) -- Point, edges, and normal
           deriving (Show, Eq)

infinityAABB :: Shape
infinityAABB = (AABB identity (V3 infinity infinity infinity) (V3 (-infinity) (-infinity) (-infinity)))

transformShape :: M44 Double -> M44 Double -> Shape -> Shape

transformShape worldToView normalMatrix (Plane planePoint planeNormal) =
    let (V4 nx ny nz nw) = normalMatrix !* (vector planeNormal)
    in Plane (P (normalizePoint (worldToView !* (point (unP planePoint))))) (V3 nx ny nz)
    
transformShape worldToView _ (Sphere sphereCenter sphereRadius) =
    Sphere (P (normalizePoint (worldToView !* (point (unP sphereCenter))))) sphereRadius

transformShape worldToView _ (AABB frame minBound maxBound) =
    AABB (worldToView !*! frame) minBound maxBound

transformShape worldToView normalMatrix (Triangle v0 v1 v2 n) =
    let (V4 nx ny nz nw) = normalMatrix !* (vector n)
    in Triangle (P (normalizePoint (worldToView !* (point (unP v0))))) (P (normalizePoint (worldToView !* (point (unP v1))))) (P (normalizePoint (worldToView !* (point (unP v2))))) (V3 nx ny nz)

transformShape worldToView normalMatrix (Disk p n r) =
    let (V4 nx ny nz nw) = normalMatrix !* (vector n)
    in Disk (P (normalizePoint (worldToView !* (point (unP p))))) (V3 nx ny nz) r

transformShape worldToView normalMatrix (Rectangle p e0 e1 n) =
    let (V4 nx ny nz nw) = normalMatrix !* (vector n)
        (V4 e0x e0y e0z e0w) = worldToView !* (vector e0)
        (V4 e1x e1y e1z e1w) = worldToView !* (vector e1)
    in Rectangle (P (normalizePoint (worldToView !* (point (unP p))))) (V3 e0x e0y e0z) (V3 e1x e1y e1z) (V3 nx ny nz)


getShapeBoundingBox :: Shape -> Shape

getShapeBoundingBox (Plane _ _) = AABB identity (V3 (-infinity) (-infinity) (-infinity)) (V3 infinity infinity infinity)

getShapeBoundingBox (Sphere (P center) radius) = AABB identity (center ^-^ (pure radius)) (center ^+^ (pure radius))

getShapeBoundingBox (AABB frame minBound maxBound) = AABB frame minBound maxBound

getShapeBoundingBox (Triangle (P v0) (P v1) (P v2) _) =
    let minBBoxValue = (\b0 b1 b2 -> min (min b0 b1) b2) <$> v0 <*> v1 <*> v2
        maxBBoxValue = (\b0 b1 b2 -> max (max b0 b1) b2) <$> v0 <*> v1 <*> v2
    in AABB identity minBBoxValue maxBBoxValue

getShapeBoundingBox (Disk (P center) _ radius) = AABB identity (center ^-^ (pure radius)) (center ^+^ (pure radius))

getShapeBoundingBox (Rectangle (P p) e0 e1 _) =
    let minBBoxValue = (\x0 x1 x2 x3 -> min (min (min x0 x1) x2) x3) <$> p <*> (p ^+^ e0) <*> (p ^+^ e1) <*> (p ^+^ e0 ^+^ e1)
        maxBBoxValue = (\x0 x1 x2 x3 -> max (max (max x0 x1) x2) x3) <$> p <*> (p ^+^ e0) <*> (p ^+^ e1) <*> (p ^+^ e0 ^+^ e1)
    in AABB identity minBBoxValue maxBBoxValue

mergeBoundingBoxes :: Shape -> Shape -> Maybe Shape
mergeBoundingBoxes (AABB frame0 minBound0 maxBound0) (AABB frame1 minBound1 maxBound1) =
    if frame0 /= frame1
    then Nothing
    else Just (AABB frame0 (min <$> minBound0 <*> minBound1) (max <$> maxBound0 <*> maxBound1))

splitBoundingBox :: V3 Double -> Shape -> (Shape, Shape)
splitBoundingBox split (AABB frame minBound maxBound) =
    let newMinBound = (\s minB -> if s /= infinity then s else minB) <$> split <*> minBound
        newMaxBound = (\s maxB -> if s /= infinity then s else maxB) <$> split <*> maxBound
    in (AABB frame minBound newMaxBound, AABB frame newMinBound maxBound)

boundingBoxSurfaceArea :: Shape -> Double
boundingBoxSurfaceArea (AABB _ minBound maxBound) =
    let (V3 dx dy dz) = maxBound ^-^ minBound
    in ((dx * dy) + (dx * dz) + (dy * dz)) * 2

