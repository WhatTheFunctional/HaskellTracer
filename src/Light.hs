module Light
    ( Light (..)
    , transformLight
    , getLightRay
    ) where

import Color
import Ray

import Linear
import Linear.Affine

data Light f c = AmbientLight c
               | PointLight (Point V3 f) c
               deriving (Show, Eq)

transformLight :: (Color c, Ord f, Floating f) => M44 f -> M44 f -> Light f c -> Light f c

transformLight _ _ (AmbientLight lightColor) =
    AmbientLight lightColor

transformLight worldToView _ (PointLight lightPoint lightColor) =
    PointLight (P (normalizePoint (worldToView !* (point (unP lightPoint))))) lightColor

getLightRay :: (Color c, Epsilon f, Floating f, Ord f, RealFloat f) => Light f c -> Point V3 f -> Ray f
getLightRay (AmbientLight _) position = Ray {rayOrigin = position, rayDirection = (V3 0.0 0.0 1.0)} -- TODO: When sampling is added, this will be a spherical direction
getLightRay (PointLight lightPosition _) position = Ray {rayOrigin = position, rayDirection = normalize (lightPosition .-. position)}
    
