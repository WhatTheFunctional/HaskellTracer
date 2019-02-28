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

data Object f c = Object (Shape f) (Material f c) (Intersection f -> Material f c -> Ray f -> Ray f -> c)

instance (Show f, Show c) => Show (Object f c) where
    show (Object shape material shaderFunction) = "Object (" ++ (show shape) ++ ") (" ++ (show material) ++ ")"

transformObject :: (Floating f, Ord f) => M44 f -> M44 f -> Object f c -> Object f c
transformObject viewToWorld normalMatrix (Object shape material shader) = Object (transformShape viewToWorld normalMatrix shape) material shader

