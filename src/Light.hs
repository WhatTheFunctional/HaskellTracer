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
import Sampling

data Light f = EnvironmentLight (Color f)
               | PointLight (Point V3 f) (Color f)
               | DirectionalLight (V3 f) (Color f)
               | DiskLight (Point V3 f) (V3 f) f (Color f) -- Point, normal, and radius
               | SphereLight (Point V3 f) f (Color f) -- Point and radius
               | RectangleLight (Point V3 f) (V3 f) (V3 f) (Color f) -- Point and radius
               deriving (Show, Eq)

transformLight :: (Floating f) => M44 f -> M44 f -> Light f -> Light f

transformLight _ _ (EnvironmentLight lightColor) =
    EnvironmentLight lightColor

transformLight worldToView _ (PointLight lightPoint lightColor) =
    PointLight (P (normalizePoint (worldToView !* (point (unP lightPoint))))) lightColor

transformLight worldToView _ (DirectionalLight lightDirection lightColor) =
    let (V4 dx dy dz dw) = worldToView !* (vector lightDirection)
    in DirectionalLight (V3 dx dy dz) lightColor

transformLight worldToView _ (DiskLight lightPoint lightDirection lightRadius lightColor) =
    let newPoint = (P (normalizePoint (worldToView !* (point (unP lightPoint)))))
        (V4 dx dy dz dw) = worldToView !* (vector lightDirection)
    in DiskLight newPoint (V3 dx dy dz) lightRadius lightColor

transformLight worldToView _ (SphereLight lightPoint lightRadius lightColor) =
    let newPoint = (P (normalizePoint (worldToView !* (point (unP lightPoint)))))
    in SphereLight newPoint lightRadius lightColor

transformLight worldToView _ (RectangleLight lightPoint lightW lightH lightColor) =
    let newPoint = (P (normalizePoint (worldToView !* (point (unP lightPoint)))))
        (V4 wx wy wz ww) = worldToView !* (vector lightW)
        (V4 hx hy hz hw) = worldToView !* (vector lightH)
    in RectangleLight newPoint (V3 wx wy wz) (V3 hx hy hz) lightColor

getLightColor :: Light f -> Color f
getLightColor (EnvironmentLight color) = color
getLightColor (PointLight _ color) = color
getLightColor (DirectionalLight _ color) = color
getLightColor (DiskLight _ _ _ color) = color
getLightColor (SphereLight _ _ color) = color
getLightColor (RectangleLight _ _ _ color) = color

getLightRay :: (Epsilon f, RealFloat f, LowDiscrepancySequence s i f) => Point V3 f -> Light f -> s i f -> ((Ray f, f), s i f) -- Ray, distance to light

getLightRay position (EnvironmentLight _) gen0 = 
    let ((phi, theta), gen1) = sampleSphere gen0
        sinPhi = sin phi
        cosPhi = cos phi
        sinTheta = sin theta
        cosTheta = cos theta
    in ((Ray {rayOrigin = position, rayDirection = (V3 (sinTheta * cosPhi) (sinTheta * sinPhi) cosTheta)}, maxValue), gen1)

getLightRay position (PointLight lightPosition _) gen =
    let rayDirection = lightPosition .-. position
    in ((Ray {rayOrigin = position, rayDirection = normalize rayDirection}, sqrt (rayDirection `dot` rayDirection)) , gen)

getLightRay position (DirectionalLight lightDirection _) gen = ((Ray {rayOrigin = position, rayDirection = normalize (-lightDirection)}, maxValue), gen)

getLightRay position (DiskLight lightPoint lightDirection lightRadius _) gen0 =
    let ((theta, r), gen1) = sampleDisk lightRadius gen0
        up = if lightDirection == (V3 0 1 0) then (V3 0 0 1) else (V3 0 1 0)
        right = normalize (up `cross` lightDirection)
        lightP = (unP lightPoint) ^+^ ((up ^* (sin theta) + right ^* (cos theta)) ^* lightRadius)
        lightD = lightP ^-^ (unP position)
    in ((Ray {rayOrigin = position, rayDirection = normalize (lightD)}, lightD `dot` lightD), gen1)

getLightRay position (SphereLight lightPoint lightRadius _) gen0 =
    let ((phi, theta), gen1) = sampleSphere gen0
        sinPhi = sin phi
        cosPhi = cos phi
        sinTheta = sin theta
        cosTheta = cos theta
        lightP = (unP lightPoint) ^+^ (V3 (sinTheta * cosPhi) (sinTheta * sinPhi) cosTheta) ^* lightRadius
        lightD = lightP ^-^ (unP position)
    in ((Ray {rayOrigin = position, rayDirection = normalize (lightD)}, lightD `dot` lightD), gen1)

getLightRay position (RectangleLight lightPoint lightW lightH _) gen0 =
    let ((x, y), gen1) = sampleRectangle 1 1 gen0
        lightP = (unP lightPoint) ^+^ (lightW ^* x) ^+^ (lightH ^* y)
        lightD = lightP ^-^ (unP position)
    in ((Ray {rayOrigin = position, rayDirection = normalize (lightD)}, lightD `dot` lightD), gen1)

