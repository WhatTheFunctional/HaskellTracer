module Object
    ( Object (..)
    , transformObject
    ) where

import Linear
import Linear.Affine

import Geometry
import Color

data Object f c = ColorObject (Shape f) c deriving (Show, Eq)

transformObject :: (Floating f, Ord f, Epsilon f) => M44 f -> M44 f -> Object f c -> Object f c
transformObject viewToWorld worldToView (ColorObject shape color) = ColorObject (transformShape viewToWorld worldToView shape) color

