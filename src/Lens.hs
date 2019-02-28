module Lens
    ( orthoLensSingle
    ) where

import Linear
import Linear.Affine

import Ray

orthoLensSingle :: (Num f) => f -> f -> Ray f
orthoLensSingle x y = Ray {rayOrigin = P (V3 x y 0), rayDirection = (V3 0 0 1)}

