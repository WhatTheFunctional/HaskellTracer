module Lens
    ( orthoLens
    ) where

import Linear
import Linear.Affine

import Ray

orthoLens :: (Num f) => f -> f -> Ray f
orthoLens x y = Ray {rayOrigin = P (V3 x y 0), rayDirection = (V3 0 0 1)}

