module Lens
    ( orthoLensSingle
    ) where

import Linear
import Linear.Affine

import Ray

orthoLensSingle :: (Epsilon f, Ord f, Floating f, RealFloat f) => f -> f -> [Ray f]
orthoLensSingle x y = [Ray {rayOrigin = P (V3 x y 0.0), rayDirection = (V3 0.0 0.0 1.0)}]

