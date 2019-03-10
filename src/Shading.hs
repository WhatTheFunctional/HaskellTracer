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
data ShadePoint = ShadePoint Material (V3 Double) (V3 Double) (V3 Double)
                deriving (Show, Eq)

shadeLight :: Material
           -> Point V3 Double
           -> V3 Double
           -> (ShadePoint -> Color Double)
           -> Ray
           -> Ray
           -> Color Double
           -> Color Double

shadeLight (ColorMaterial color) point normal shader ray@(Ray {rayDirection = rd}) lightRay@(Ray {rayDirection = lrd}) lightColor =
   shader (ShadePoint (ColorMaterial color) normal lrd rd)

shadeLight material point normal shader ray@(Ray {rayDirection = rd}) lightRay@(Ray {rayDirection = lrd}) lightColor =
   let ndotrd = normal `dot` lrd
   in if ndotrd < 0
      then pure 0
      else ((shader (ShadePoint material normal lrd (-rd))) ^*^ lightColor) ^* ndotrd

-- Shaders compute f

colorShader :: ShadePoint -> Color Double
colorShader (ShadePoint (ColorMaterial color) normal wIn wOut) = color
colorShader (ShadePoint (MatteMaterial diffuse _) normal wIn wOut) = diffuse
colorShader (ShadePoint (PlasticMaterial diffuse _ _ _ _) normal wIn wOut) = diffuse

diffuseF :: Color Double
         -> Double
         -> Color Double
diffuseF diffuse kD =
    let invPi = 1.0 / pi
    in diffuse ^* (kD * invPi)

lambertShader :: ShadePoint -> Color Double
lambertShader (ShadePoint (ColorMaterial color) normal wIn wOut) = color
lambertShader (ShadePoint (MatteMaterial diffuse kD) normal wIn wOut) = diffuseF diffuse kD
lambertShader (ShadePoint (PlasticMaterial diffuse kD _ _ _) normal wIn wOut) = diffuseF diffuse kD

specularF :: Color Double
          -> Double
          -> Double
          -> V3 Double
          -> V3 Double
          -> V3 Double
          -> Color Double
specularF specular kS kExp normal wIn wOut =
    let ndotwi = normal `dot` wIn
        r = (-wIn) ^+^ (normal ^* (ndotwi * 2))
        rdotwo = r `dot` wOut
    in if rdotwo > 0
       then specular ^* (kS * (rdotwo ** kExp))
       else pure 0

phongShader :: ShadePoint -> Color Double
phongShader (ShadePoint (ColorMaterial color) normal wIn wOut) = color
phongShader (ShadePoint (MatteMaterial diffuse kD) normal wIn wOut) = diffuseF diffuse kD
phongShader (ShadePoint (PlasticMaterial diffuse kD specular kS kExp) normal wIn wOut) = (diffuseF diffuse kD) ^+^ (specularF specular kS kExp normal wIn wOut)

