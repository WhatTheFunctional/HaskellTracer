module Shading
    ( colorShader
    ) where

import Linear

import Ray
import Color
import Material

colorShader :: (Color c, Floating f, Ord f) => Intersection f -> Material f c -> c
colorShader intersection (ColorMaterial color) = color
colorShader intersection (MatteMaterial diffuse) = diffuse
colorShader intersection (PlasticMaterial diffuse specular) = diffuse

