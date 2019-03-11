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


class LowDiscrepancySequence s where
    sample :: s -> (Double, s) -- Range of [0, 1]
    sampleR :: (Double, Double) -> s -> (Double, s)

data Halton = Halton Int Int [Double] -- Index, count, cache
            deriving (Show, Eq)

haltonIteration :: Int -> Int -> Double -> Double -> Double
haltonIteration !base !index !fraction !result =
    if index <= 0
    then (max 0 (min result 1))
    else let fractionNew = fraction / (fromIntegral base)
             resultNew = result + fractionNew * (fromIntegral (index `mod` base))
             indexNew = fromIntegral (floor ((fromIntegral index) / (fromIntegral base)))
         in haltonIteration base indexNew fractionNew resultNew

mkHaltonCache :: Int -> Int -> ([Double], Int)
mkHaltonCache count base = ([haltonIteration base x 1 0 | x <- [503..(503 + count - 1)]], count)

mkHaltonLDS :: ([Double], Int) -> Int -> Halton
mkHaltonLDS (!cache, !count) !index =
    let newIndex = (index `mod` count)
    in Halton newIndex count cache

instance LowDiscrepancySequence Halton where
    sample (Halton index count cache) = (cache !! (fromIntegral index), Halton ((index + 1) `mod` count) count cache)
    sampleR (minRange, maxRange) (Halton index count cache) = ((cache !! (fromIntegral index)) * (maxRange - minRange) + minRange, Halton ((index + 1) `mod` count) count cache)

sampleRectangle :: (LowDiscrepancySequence s) => Double -> Double -> s -> ((Double, Double), s)
sampleRectangle w h gen0 =
    let (x, gen1) = sampleR (0, w) gen0
        (y, gen2) = sampleR (0, h) gen1
    in ((x, y), gen2)

sampleDisk :: (LowDiscrepancySequence s) => Double -> s -> ((Double, Double), s)
sampleDisk radius gen0 =
    let twoPi = pi * 2
        (theta, gen1) = sampleR (0, twoPi) gen0
        (r, gen2) = sampleR (0, radius) gen1
    in ((theta, r), gen2)

sampleHemisphere :: (LowDiscrepancySequence s) => s -> ((Double, Double), s)
sampleHemisphere gen0 =
    let twoPi = pi * 2
        halfPi = pi / 2
        (phi, gen1) = sampleR (0, twoPi) gen0
        (theta, gen2) = sampleR (0, halfPi) gen1
    in ((phi, theta), gen2)

sampleSphere :: (LowDiscrepancySequence s) => s -> ((Double, Double), s)
sampleSphere gen0 =
    let twoPi = pi * 2
        (phi, gen1) = sampleR (0, twoPi) gen0
        (theta, gen2) = sampleR (0, pi) gen1
    in ((phi, theta), gen2)

singleSampling :: (LowDiscrepancySequence s)
               => (Double -> Double -> Double -> Double -> Ray)
               -> Double -> Double -> Double -> Double -> Double -> s -> ([Ray], s)
singleSampling lensFunction pixelSize w h x y gen = ([lensFunction w h x y], gen)

grid4xSampling :: (LowDiscrepancySequence s)
               => (Double -> Double -> Double -> Double -> Ray)
               -> Double -> Double -> Double -> Double -> Double -> s -> ([Ray], s)
grid4xSampling lensFunction pixelSize w h x y gen =
    let offset = realToFrac (pixelSize * 0.25)
    in ([ lensFunction w h (x - offset) (y - offset)
        , lensFunction w h (x + offset) (y - offset)
        , lensFunction w h (x - offset) (y + offset)
        , lensFunction w h (x + offset) (y + offset)
        ], gen)

randomSampling :: (LowDiscrepancySequence s)
               => Int
               -> (Double -> Double -> Double -> Double -> Ray)
               -> Double -> Double -> Double -> Double -> Double -> s -> ([Ray], s)
randomSampling count lensFunction pixelSize w h x y gen =
    let halfSampleSize = pixelSize
        sampleSize = pixelSize * 2
        samplesIndices = [0..(count - 1)]
    in foldl' (\(accumulator, gen1) c ->
                 let ((rayX, rayY), gen2) = sampleRectangle sampleSize sampleSize gen1
                     ray = lensFunction w h (x + rayX - halfSampleSize) (y + rayY - halfSampleSize)
                 in (ray : accumulator, gen2)) ([], gen) samplesIndices

