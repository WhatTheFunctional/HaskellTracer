module Shading
    ( shadeAllLights
    , colorShader
    ) where

import Linear

import Ray
import Color
import Material
import Light

shadeAllLights :: (Color c, Epsilon f, Floating f, Ord f, RealFloat f) => Intersection f -> Material f c -> (Intersection f -> Material f c -> Ray f -> Ray f -> c) -> Ray f -> [Light f c] -> c
shadeAllLights intersection@(Intersection {intersectionPoint = point}) material shader ray lights =
    foldr (\light accumulatedColor -> mixColors (shader intersection material ray (getLightRay light point)) accumulatedColor) blankColor lights

-- Shaders compute rho

colorShader :: (Color c, Epsilon f, Floating f, Ord f, RealFloat f) => Intersection f -> Material f c -> Ray f -> Ray f -> c
colorShader intersection (ColorMaterial color) rayIn rayOut = color
colorShader intersection (MatteMaterial diffuse) rayIn rayOut = diffuse
colorShader intersection (PlasticMaterial diffuse specular) rayIn rayOut = diffuse

