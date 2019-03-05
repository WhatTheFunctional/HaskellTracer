module Sampling
    ( HaltonGen (..)
    , sampleQuad
    , sampleDisk
    , sampleHemisphere
    , sampleSphere
    , singleSampling
    , grid4xSampling
    ) where

import System.Random

import Lens
import Ray

data HaltonGen i = HaltonGen i i i i -- Base, index, rangeMin, rangeMax
              deriving (Show, Eq)

haltonIteration :: (Fractional f, Integral i) => i -> i -> f -> f -> f
haltonIteration base index fraction result =
    if index > 0
    then let fractionNew = fraction / (fromIntegral base)
             resultNew = result + fractionNew * (fromIntegral (index `mod` base))
             indexNew = fromIntegral (floor ((fromIntegral index) / (fromIntegral base)))
         in haltonIteration base indexNew fractionNew resultNew
    else result

instance (Integral i) => RandomGen (HaltonGen i) where
    next (HaltonGen base index rangeMin rangeMax) =
        (fromIntegral $ rangeMin + (floor ((haltonIteration base index 1 0) * (fromIntegral (rangeMax - rangeMin)))),
         HaltonGen base (index + 1) rangeMin rangeMax)
    genRange (HaltonGen base index rangeMin rangeMax) = (fromIntegral rangeMin, fromIntegral rangeMax)
    split (HaltonGen base index rangeMin rangeMax) =
        (HaltonGen (base + 1) index rangeMin rangeMax, HaltonGen (base + 2) index rangeMin rangeMax)

sampleQuad :: (Num f, Random f, RandomGen g) => f -> f -> g -> ((f, f), g)
sampleQuad w h gen0 =
    let (x, gen1) = randomR (0, w) gen0
        (y, gen2) = randomR (0, h) gen1
    in ((x, y), gen2)

sampleDisk :: (Floating f, Random f, RandomGen g) => f -> g -> ((f, f), g)
sampleDisk radius gen0 =
    let (theta, gen1) = randomR (-pi, pi) gen0
        (r, gen2) = randomR (0, radius) gen1
    in ((theta, r), gen2)

sampleHemisphere :: (Floating f, Random f, RandomGen g) => g -> ((f, f), g)
sampleHemisphere gen0 =
    let halfPi = pi / 2
        (phi, gen1) = randomR (-pi, pi) gen0
        (theta, gen2) = randomR (0, halfPi) gen1
    in ((phi, theta), gen2)

sampleSphere :: (Floating f, Random f, RandomGen g) => g -> ((f, f), g)
sampleSphere gen0 =
    let (phi, gen1) = randomR (-pi, pi) gen0
        (theta, gen2) = randomR (0, pi) gen1
    in ((phi, theta), gen2)

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

