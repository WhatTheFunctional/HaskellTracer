module Light
    ( Light (..)
    , transformLight
    , getLightColor
    , getLightRay
    ) where

import Numeric.Limits
import Linear
import Linear.Affine

import Color
import Ray

data Light f = EnvironmentLight (Color f)
               | PointLight (Point V3 f) (Color f)
               | DirectionalLight (V3 f) (Color f)
               deriving (Show, Eq)

transformLight :: (Floating f) => M44 f -> M44 f -> Light f -> Light f

transformLight _ _ (EnvironmentLight lightColor) =
    EnvironmentLight lightColor

transformLight worldToView _ (PointLight lightPoint lightColor) =
    PointLight (P (normalizePoint (worldToView !* (point (unP lightPoint))))) lightColor

transformLight worldToView _ (DirectionalLight lightDirection lightColor) =
    let (V4 nx ny nz nw) = worldToView !* (vector lightDirection)
    in DirectionalLight (V3 nx ny nz) lightColor

getLightColor :: Light f -> Color f
getLightColor (EnvironmentLight color) = color
getLightColor (PointLight _ color) = color
getLightColor (DirectionalLight _ color) = color

getLightRay :: (Epsilon f, RealFloat f) => Point V3 f -> Light f -> (Ray f, f) -- Ray, distance to light

getLightRay position (EnvironmentLight _) = (Ray {rayOrigin = position, rayDirection = (V3 0 0 1)}, maxValue) -- TODO: When sampling is added, this will be a spherical direction

getLightRay position (PointLight lightPosition _) = 
    let rayDirection = lightPosition .-. position
    in (Ray {rayOrigin = position, rayDirection = normalize rayDirection}, sqrt (rayDirection `dot` rayDirection)) 

getLightRay position (DirectionalLight lightDirection _) = (Ray {rayOrigin = position, rayDirection = normalize (-lightDirection)}, maxValue)
    
