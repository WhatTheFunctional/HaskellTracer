module Geometry
    ( Shape (..)
    , transformShape
    ) where

import Linear
import Linear.Affine

data Shape f = Plane (Point V3 f) (V3 f) -- Point and normal
             | Sphere (Point V3 f) f -- Origin and radius
             deriving (Show, Eq)

transformShape :: (Ord f, Floating f) => M44 f -> M44 f -> Shape f -> Shape f

transformShape worldToView normalMatrix (Plane planePoint planeNormal) =
    let (V4 nx ny nz nw) = normalMatrix !* (vector planeNormal)
    in Plane (P (normalizePoint (worldToView !* (point (unP planePoint))))) (V3 nx ny nz)
    
transformShape worldToView _ (Sphere sphereOrigin sphereRadius) =
    Sphere (P (normalizePoint (worldToView !* (point (unP sphereOrigin))))) sphereRadius

