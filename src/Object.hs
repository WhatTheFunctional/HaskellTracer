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

data Object = Object Shape Material (ShadePoint -> Color Double)

instance Show Object where
    show (Object shape material shaderFunction) = "Object (" ++ (show shape) ++ ") (" ++ (show material) ++ ")"

transformObject :: M44 Double -> M44 Double -> Object -> Object
transformObject viewToWorld normalMatrix (Object shape material shader) = Object (transformShape viewToWorld normalMatrix shape) material shader

