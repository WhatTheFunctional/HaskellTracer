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
             deriving (Show, Eq)

transformShape :: (Fractional f) => M44 f -> M44 f -> Shape f -> Shape f

transformShape worldToView normalMatrix (Plane planePoint planeNormal) =
    let (V4 nx ny nz nw) = normalMatrix !* (vector planeNormal)
    in Plane (P (normalizePoint (worldToView !* (point (unP planePoint))))) (V3 nx ny nz)
    
transformShape worldToView _ (Sphere sphereCenter sphereRadius) =
    Sphere (P (normalizePoint (worldToView !* (point (unP sphereCenter))))) sphereRadius

transformShape worldToView _ (AABB frame minBound maxBound) =
    AABB (worldToView !*! frame) minBound maxBound


getShapeBoundingBox :: (RealFloat f) => Shape f -> Shape f

getShapeBoundingBox (Plane _ _) = AABB identity (V3 (-maxValue) (-maxValue) (-maxValue)) (V3 maxValue maxValue maxValue)

getShapeBoundingBox (Sphere (P center) radius) = AABB identity (center ^-^ (pure radius)) (center ^+^ (pure radius))
getShapeBoundingBox (AABB frame minBound maxBound) = AABB frame minBound maxBound

