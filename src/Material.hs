module Material
    ( Material (..)
    ) where

import Color

data Material f = ColorMaterial (Color f) -- Color (No shading)
                  | MatteMaterial (Color f) f -- Diffuse, kD
                  | PlasticMaterial (Color f) f (Color f) f -- Diffuse, kD, Specular, kS
                  deriving (Show, Eq)

