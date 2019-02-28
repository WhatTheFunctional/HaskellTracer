module Light
    ( Light (..)
    , transformLight
    , getLightColor
    , getLightRay
    ) where

import Color
import Ray

import Linear
import Linear.Affine

data Light f = EnvironmentLight (Color f)
               | PointLight (Point V3 f) (Color f)
               deriving (Show, Eq)

transformLight :: (Floating f) => M44 f -> M44 f -> Light f -> Light f

transformLight _ _ (EnvironmentLight lightColor) =
    EnvironmentLight lightColor

transformLight worldToView _ (PointLight lightPoint lightColor) =
    PointLight (P (normalizePoint (worldToView !* (point (unP lightPoint))))) lightColor

getLightColor :: Light f -> Color f
getLightColor (EnvironmentLight color) = color
getLightColor (PointLight _ color) = color

getLightRay :: (Epsilon f, Floating f) => Light f -> Point V3 f -> Ray f
--getLightRay (EnvironmentLight _) position = Ray {rayOrigin = position, rayDirection = (V3 0 0 1)} -- TODO: When sampling is added, this will be a spherical direction
getLightRay (PointLight lightPosition _) position = Ray {rayOrigin = position, rayDirection = normalize (lightPosition .-. position)}
    
