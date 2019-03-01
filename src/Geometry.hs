module Geometry
    ( Shape (..)
    , transformShape
    , getShapeBoundingBox
    ) where

import Numeric.Limits
import Linear
import Linear.Affine
import Linear.Matrix

data Shape f = Plane (Point V3 f) (V3 f) -- Point and normal
             | Sphere (Point V3 f) f -- Center and radius
             | AABB (M44 f) (V3 f) (V3 f) -- Frame, min, and max
             | Triangle (Point V3 f) (Point V3 f) (Point V3 f) (V3 f) -- Points, normal
             deriving (Show, Eq)

transformShape :: (Fractional f) => M44 f -> M44 f -> Shape f -> Shape f

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


getShapeBoundingBox :: (RealFloat f) => Shape f -> Shape f

getShapeBoundingBox (Plane _ _) = AABB identity (V3 (-maxValue) (-maxValue) (-maxValue)) (V3 maxValue maxValue maxValue)

getShapeBoundingBox (Sphere (P center) radius) = AABB identity (center ^-^ (pure radius)) (center ^+^ (pure radius))

getShapeBoundingBox (AABB frame minBound maxBound) = AABB frame minBound maxBound

getShapeBoundingBox (Triangle (P v0) (P v1) (P v2) _) =
    let minValue = (\b0 b1 b2 -> min (min b0 b1) b2) <$> v0 <*> v1 <*> v2
        maxValue = (\b0 b1 b2 -> max (max b0 b1) b2) <$> v0 <*> v1 <*> v2
    in AABB identity minValue maxValue

