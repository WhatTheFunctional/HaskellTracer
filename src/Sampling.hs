module Sampling
    ( halton
    , singleSampling
    , grid4xSampling
    ) where

import Lens
import Ray

haltonIteration :: (Integral i, Fractional r) => i -> i -> r -> r -> r
haltonIteration base index f result =
    if index > 0
    then let fNew = f / (fromIntegral base)
             resultNew = result + fNew * (fromIntegral (index `mod` base))
             indexNew = fromIntegral (floor ((fromIntegral index) / (fromIntegral base)))
         in haltonIteration base indexNew fNew resultNew
    else (result, index + 1)

halton :: (Integral i, Fractional r) => i -> i -> (r, i)
halton base index = haltonIteration base index 1 0

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

