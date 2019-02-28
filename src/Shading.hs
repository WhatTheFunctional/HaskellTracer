module Shading
    ( shadeAllLights
    , colorShader
    ) where

import Linear
import Linear.Affine

import Ray
import Color
import Material
import Light

shadeAllLights :: (Epsilon f, Floating f, Ord f) => Intersection f -> Material f -> (V3 f -> Material f -> V3 f -> V3 f -> Color f) -> Ray f -> [Light f] -> Color f

shadeAllLights intersection@(Intersection {intersectionPoint = point, intersectionNormal = normal}) (ColorMaterial color) shader ray@(Ray {rayDirection = rd}) lights =
    foldr (\light accumulatedColor ->
               let lightRay@(Ray {rayDirection = lrd}) = getLightRay light point
               in (shader normal (ColorMaterial color) lrd rd) ^+^ accumulatedColor) (pure 0) lights

shadeAllLights intersection@(Intersection {intersectionPoint = point, intersectionNormal = normal}) material shader ray@(Ray {rayDirection = rd}) lights =
    foldr (\light accumulatedColor ->
               let lightRay@(Ray {rayDirection = lrd}) = getLightRay light point
                   ndotrd = normal `dot` lrd
               in if ndotrd < 0
                  then accumulatedColor
                  else (((shader normal material lrd rd) ^*^ (getLightColor light)) ^* ndotrd) ^+^ accumulatedColor) (pure 0) lights

-- Shaders compute f

colorShader :: V3 f -> Material f -> V3 f -> V3 f -> Color f
colorShader normal (ColorMaterial color) wIn wOut = color
colorShader normal (MatteMaterial diffuse _) wIn wOut = diffuse
colorShader normal (PlasticMaterial diffuse _ _ _) wIn wOut = diffuse

lambertF :: (Floating f) => Color f -> f -> Color f
lambertF diffuse kD =
    let invPi = 1.0 / pi
    in diffuse ^* (kD * invPi)

lambertShader :: (Floating f) => V3 f -> Material f -> V3 f -> V3 f -> Color f
lambertShader normal (ColorMaterial color) wIn wOut = color
lambertShader normal (MatteMaterial diffuse kD) wIn wOut = lambertF diffuse kD
lambertShader normal (PlasticMaterial diffuse kD _ _) wIn wOut = lambertF diffuse kD

