module Object
    ( Object (..)
    , transformObject
    ) where

import Linear

import Geometry
import Color

data Object f c = ColorObject (Shape f) c deriving (Show, Eq)

transformObject :: (Floating f, Ord f) => M44 f -> M44 f -> Object f c -> Object f c
transformObject viewToWorld normalMatrix (ColorObject shape color) = ColorObject (transformShape viewToWorld normalMatrix shape) color

