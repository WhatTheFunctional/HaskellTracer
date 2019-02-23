module Geometry
    ( Shape (..)
    , transformShape
    ) where

import Linear
import Linear.Epsilon
import Linear.Affine

data Shape f = Plane (Point V3 f) (V3 f) -- Point and normal
             | Sphere (Point V3 f) f -- Origin and radius
             deriving (Show, Eq)

transformShape :: (Ord f, Epsilon f, Floating f) => M44 f -> M44 f -> Shape f -> Shape f

transformShape viewToWorld worldToView (Plane planePoint planeNormal) =
    Plane (P (normalizePoint (worldToView !* (point (unP planePoint))))) (normalizePoint ((vector planeNormal) *! viewToWorld))
    
transformShape _ worldToView (Sphere sphereOrigin sphereRadius) =
    Sphere (P (normalizePoint (worldToView !* (point (unP sphereOrigin))))) sphereRadius

