module Shading
    ( ShadePoint (..)
    , shadeLight
    , colorShader
    , lambertShader
    ) where

import Linear
import Linear.Affine

import Ray
import Color
import Material
import Light

-- Contains all information required to shade a surface point
data ShadePoint f = ShadePoint (Material f) (V3 f) (V3 f) (V3 f) deriving (Show, Eq)

shadeLight :: (Epsilon f, Floating f, Ord f) => Material f -> Point V3 f -> V3 f -> (ShadePoint f -> Color f) -> Ray f -> Ray f -> Color f -> Color f

shadeLight (ColorMaterial color) point normal shader ray@(Ray {rayDirection = rd}) lightRay@(Ray {rayDirection = lrd}) lightColor =
   shader (ShadePoint (ColorMaterial color) normal lrd rd)

shadeLight material point normal shader ray@(Ray {rayDirection = rd}) lightRay@(Ray {rayDirection = lrd}) lightColor =
   let ndotrd = normal `dot` lrd
   in if ndotrd < 0
      then pure 0
      else ((shader (ShadePoint material normal lrd rd)) ^*^ lightColor) ^* ndotrd

-- Shaders compute f

colorShader :: ShadePoint f -> Color f
colorShader (ShadePoint (ColorMaterial color) normal wIn wOut) = color
colorShader (ShadePoint (MatteMaterial diffuse _) normal wIn wOut) = diffuse
colorShader (ShadePoint (PlasticMaterial diffuse _ _ _) normal wIn wOut) = diffuse

lambertF :: (Floating f) => Color f -> f -> Color f
lambertF diffuse kD =
    let invPi = 1.0 / pi
    in diffuse ^* (kD * invPi)

lambertShader :: (Floating f) => ShadePoint f -> Color f
lambertShader (ShadePoint (ColorMaterial color) normal wIn wOut) = color
lambertShader (ShadePoint (MatteMaterial diffuse kD) normal wIn wOut) = lambertF diffuse kD
lambertShader (ShadePoint (PlasticMaterial diffuse kD _ _) normal wIn wOut) = lambertF diffuse kD

