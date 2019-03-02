module Sampling
    ( singleSampling
    , grid4xSampling
    ) where

import Lens
import Ray

singleSampling :: (f -> f -> f -> f -> Ray f) -> g -> f -> f -> f -> f -> [Ray f]
singleSampling lensFunction pixelSize w h x y = [lensFunction w h x y]

grid4xSampling :: (Fractional f, RealFrac g) => (f -> f -> f -> f -> Ray f) -> g -> f -> f -> f -> f -> [Ray f]
grid4xSampling lensFunction pixelSize w h x y =
    let offset = realToFrac (pixelSize * 0.25)
    in [ lensFunction w h (x - offset) (y - offset)
       , lensFunction w h (x + offset) (y - offset)
       , lensFunction w h (x - offset) (y + offset)
       , lensFunction w h (x + offset) (y + offset)
       ]

