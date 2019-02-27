module Light
    ( Light (..)
    , transformLight
    , getLightRay
    ) where

import Ray

import Linear
import Linear.Affine

data Light f = PointLight (Point V3 f)
             deriving (Show, Eq)

transformLight :: (Ord f, Floating f) => M44 f -> M44 f -> Light f -> Light f

transformLight worldToView _ (PointLight lightPoint) =
    PointLight (P (normalizePoint (worldToView !* (point (unP lightPoint)))))

getLightRay :: (Epsilon f, Floating f, Ord f, RealFloat f) => Light f -> Point V3 f -> Ray f
getLightRay (PointLight lightPosition) position = Ray {rayOrigin = position, rayDirection = normalize (lightPosition .-. position)}
    
