module Sampling
    ( singleSampling
    , grid4xSampling
    ) where

import Lens
import Ray

singleSampling :: (f -> f -> Ray f) -> g -> f -> f -> [Ray f]
singleSampling lensFunction pixelSize x y = [lensFunction x y]

grid4xSampling :: (Fractional f, RealFrac g) => (f -> f -> Ray f) -> g -> f -> f -> [Ray f]
grid4xSampling lensFunction pixelSize x y =
    let offset = realToFrac (pixelSize * 0.25)
    in [ lensFunction (x - offset) (y - offset)
       , lensFunction (x + offset) (y - offset)
       , lensFunction (x - offset) (y + offset)
       , lensFunction (x + offset) (y + offset)
       ]

