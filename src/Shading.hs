module Shading
    ( shadeLight
    , colorShader
    ) where

import Linear
import Linear.Affine

import Ray
import Color
import Material
import Light

shadeLight :: (Epsilon f, Floating f, Ord f) => Point V3 f -> V3 f -> Material f -> (V3 f -> Material f -> V3 f -> V3 f -> Color f) -> Ray f -> Ray f -> Color f -> Color f

shadeLight point normal (ColorMaterial color) shader ray@(Ray {rayDirection = rd}) lightRay@(Ray {rayDirection = lrd}) lightColor =
   shader normal (ColorMaterial color) lrd rd

shadeLight point normal material shader ray@(Ray {rayDirection = rd}) lightRay@(Ray {rayDirection = lrd}) lightColor =
   let ndotrd = normal `dot` lrd
   in if ndotrd < 0
      then pure 0
      else ((shader normal material lrd rd) ^*^ lightColor) ^* ndotrd

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

