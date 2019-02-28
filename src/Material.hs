module Material
    ( Material (..)
    ) where

data Material f c = ColorMaterial c -- Color (No shading)
                  | MatteMaterial c -- Diffuse
                  | PlasticMaterial c c -- Diffuse, Specular
                  deriving (Show, Eq)

