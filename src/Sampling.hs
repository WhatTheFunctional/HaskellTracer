{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
module Sampling
    ( LowDiscrepancySequence (..)
    , Halton (..)
    , mkHaltonCache
    , mkHaltonLDS
    , sampleRectangle
    , sampleDisk
    , sampleHemisphere
    , sampleSphere
    , singleSampling
    , grid4xSampling
    , randomSampling
    ) where

import Data.List
import Lens
import Ray


class LowDiscrepancySequence s i f where
    sample :: s i f -> (f, s i f) -- Range of [0, 1]
    sampleR :: (f, f) -> s i f -> (f, s i f)

data Halton i f = Halton i i [f] -- Index, count, cache
              deriving (Show, Eq)

haltonIteration :: (Fractional f, Ord f, Integral i) => i -> i -> f -> f -> f
haltonIteration !base !index !fraction !result =
    if index <= 0
    then (max 0 (min result 1))
    else let fractionNew = fraction / (fromIntegral base)
             resultNew = result + fractionNew * (fromIntegral (index `mod` base))
             indexNew = fromIntegral (floor ((fromIntegral index) / (fromIntegral base)))
         in haltonIteration base indexNew fractionNew resultNew

mkHaltonCache :: (Fractional f, Ord f, Integral i) => i -> i -> ([f], i)
mkHaltonCache count base = ([haltonIteration base x 1 0 | x <- [503..(503 + count - 1)]], count)

mkHaltonLDS :: (Fractional f, Ord f, Integral i) => ([f], i) -> i -> Halton i f
mkHaltonLDS (!cache, !count) !index =
    let newIndex = (index `mod` count)
    in Halton newIndex count cache

instance (Num f, Ord f, Integral i) => LowDiscrepancySequence Halton i f where
    sample (Halton index count cache) = (cache !! (fromIntegral index), Halton ((index + 1) `mod` count) count cache)
    sampleR (minRange, maxRange) (Halton index count cache) = ((cache !! (fromIntegral index)) * (maxRange - minRange) + minRange, Halton ((index + 1) `mod` count) count cache)

sampleRectangle :: (Fractional f, LowDiscrepancySequence s i f) => f -> f -> s i f -> ((f, f), s i f)
sampleRectangle w h gen0 =
    let (x, gen1) = sampleR (0, w) gen0
        (y, gen2) = sampleR (0, h) gen1
    in ((x, y), gen2)

sampleDisk :: (Floating f, LowDiscrepancySequence s i f) => f -> s i f -> ((f, f), s i f)
sampleDisk radius gen0 =
    let twoPi = pi * 2
        (theta, gen1) = sampleR (0, twoPi) gen0
        (r, gen2) = sampleR (0, radius) gen1
    in ((theta, r), gen2)

sampleHemisphere :: (Floating f, LowDiscrepancySequence s i f) => s i f -> ((f, f), s i f)
sampleHemisphere gen0 =
    let twoPi = pi * 2
        halfPi = pi / 2
        (phi, gen1) = sampleR (0, twoPi) gen0
        (theta, gen2) = sampleR (0, halfPi) gen1
    in ((phi, theta), gen2)

sampleSphere :: (Floating f, LowDiscrepancySequence s i f) => s i f -> ((f, f), s i f)
sampleSphere gen0 =
    let twoPi = pi * 2
        (phi, gen1) = sampleR (0, twoPi) gen0
        (theta, gen2) = sampleR (0, pi) gen1
    in ((phi, theta), gen2)

singleSampling :: (LowDiscrepancySequence s i f) => (f -> f -> f -> f -> Ray f) -> f -> f -> f -> f -> f -> s i f -> ([Ray f], s i f)
singleSampling lensFunction pixelSize w h x y gen = ([lensFunction w h x y], gen)

grid4xSampling :: (RealFrac f, LowDiscrepancySequence s i f) => (f -> f -> f -> f -> Ray f) -> f -> f -> f -> f -> f -> s i f -> ([Ray f], s i f)
grid4xSampling lensFunction pixelSize w h x y gen =
    let offset = realToFrac (pixelSize * 0.25)
    in ([ lensFunction w h (x - offset) (y - offset)
        , lensFunction w h (x + offset) (y - offset)
        , lensFunction w h (x - offset) (y + offset)
        , lensFunction w h (x + offset) (y + offset)
        ], gen)

randomSampling :: (Fractional f, LowDiscrepancySequence s i f) => Int -> (f -> f -> f -> f -> Ray f) -> f -> f -> f -> f -> f -> s i f -> ([Ray f], s i f)
randomSampling count lensFunction pixelSize w h x y gen =
    let halfSampleSize = pixelSize / 2
        sampleSize = pixelSize
        samplesIndices = [0..(count - 1)]
    in foldl' (\(accumulator, gen1) c ->
                 let ((rayX, rayY), gen2) = sampleRectangle sampleSize sampleSize gen1
                     ray = lensFunction w h (x + rayX - halfSampleSize) (y + rayY - halfSampleSize)
                 in (ray : accumulator, gen2)) ([], gen) samplesIndices

