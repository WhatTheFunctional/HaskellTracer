module Shading
    ( ShadePoint (..)
    , shadeLight
    , colorShader
    , lambertShader
    , phongShader
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
      else ((shader (ShadePoint material normal lrd (-rd))) ^*^ lightColor) ^* ndotrd

-- Shaders compute f

colorShader :: ShadePoint f -> Color f
colorShader (ShadePoint (ColorMaterial color) normal wIn wOut) = color
colorShader (ShadePoint (MatteMaterial diffuse _) normal wIn wOut) = diffuse
colorShader (ShadePoint (PlasticMaterial diffuse _ _ _ _) normal wIn wOut) = diffuse

diffuseF :: (Floating f) => Color f -> f -> Color f
diffuseF diffuse kD =
    let invPi = 1.0 / pi
    in diffuse ^* (kD * invPi)

lambertShader :: (Floating f) => ShadePoint f -> Color f
lambertShader (ShadePoint (ColorMaterial color) normal wIn wOut) = color
lambertShader (ShadePoint (MatteMaterial diffuse kD) normal wIn wOut) = diffuseF diffuse kD
lambertShader (ShadePoint (PlasticMaterial diffuse kD _ _ _) normal wIn wOut) = diffuseF diffuse kD

specularF :: (Floating f, Ord f) => Color f -> f -> f -> V3 f -> V3 f -> V3 f -> Color f
specularF specular kS kExp normal wIn wOut =
    let ndotwi = normal `dot` wIn
        r = (-wIn) ^+^ (normal ^* (ndotwi * 2))
        rdotwo = r `dot` wOut
    in if rdotwo > 0
       then specular ^* (kS * (rdotwo ** kExp))
       else pure 0

phongShader :: (Floating f, Ord f) => ShadePoint f -> Color f
phongShader (ShadePoint (ColorMaterial color) normal wIn wOut) = color
phongShader (ShadePoint (MatteMaterial diffuse kD) normal wIn wOut) = diffuseF diffuse kD
phongShader (ShadePoint (PlasticMaterial diffuse kD specular kS kExp) normal wIn wOut) = (diffuseF diffuse kD) ^+^ (specularF specular kS kExp normal wIn wOut)

