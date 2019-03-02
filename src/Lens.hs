module Lens
    ( orthoLens
    , perspectiveLens
    ) where

import Linear
import Linear.Affine

import Ray

orthoLens :: (Num f) => f -> f -> f -> f -> Ray f
orthoLens w h x y = Ray {rayOrigin = P (V3 x y 0), rayDirection = (V3 0 0 1)}

perspectiveLens :: (Epsilon f, Floating f) => f -> f -> f -> f -> f -> Ray f
perspectiveLens fov w h x y =
    let ro = (V3 x y 0)
        rd = normalize (ro - (V3 0 0 (-((abs w) * (tan fov)))))
    in Ray {rayOrigin = P ro, rayDirection = rd}

