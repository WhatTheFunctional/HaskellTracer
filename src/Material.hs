module Material
    ( Material (..)
    ) where

import Color

data Material = ColorMaterial (Color Double) -- Color (No shading)
              | MatteMaterial (Color Double) Double -- Diffuse, kD
              | PlasticMaterial (Color Double) Double (Color Double) Double Double -- Diffuse, kD, Specular, kS, kExp
              deriving (Show, Eq)

