module Object
    ( Object (..)
    , transformObject
    ) where

import Linear

import Ray
import Geometry
import Color
import Material
import Shading
import Accelerator

data Object f = Object (Shape f) (Material f) (ShadePoint f -> Color f)

instance (Show f) => Show (Object f) where
    show (Object shape material shaderFunction) = "Object (" ++ (show shape) ++ ") (" ++ (show material) ++ ")"

transformObject :: (Fractional f) => M44 f -> M44 f -> Object f -> Object f
transformObject viewToWorld normalMatrix (Object shape material shader) = Object (transformShape viewToWorld normalMatrix shape) material shader

