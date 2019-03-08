{-# LANGUAGE BangPatterns #-}
module IO
    ( writePNG
    , writeParallelPNG
    ) where

import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Parallel
import Control.Parallel.Strategies
import Codec.Picture
import Linear

import Color

clampComponent :: (RealFrac f, Integral i) => f -> i
clampComponent x = round (max 0.0 (min 255.0 (x * 255)))

toPixelRGB8 :: (RealFrac f) => Color f -> PixelRGB8
toPixelRGB8 (RGB r g b) =
    PixelRGB8 (fromIntegral (clampComponent r))
              (fromIntegral (clampComponent g))
              (fromIntegral (clampComponent b))

generatorToPixelRGB8 :: (RealFrac f) => (Int -> Int -> Color f) -> (Int -> Int -> PixelRGB8)
generatorToPixelRGB8 generator = \worldX worldY -> toPixelRGB8 (generator worldX worldY)

writePNG :: (RealFrac f) => String -> (Int, Int, (Int -> Int -> Color f)) -> IO ()
writePNG fileName (width, height, generator) =
    let pixelRGB8Generator = generatorToPixelRGB8 generator
    in savePngImage fileName (ImageRGB8 (generateImage pixelRGB8Generator width height))

generatorTuple :: (RealFrac f) => (Int -> Int -> Color f) -> (Int, Int) -> Color f
generatorTuple generator (x, y) = generator x y

colorToPixelRGB8 :: (RealFrac f) => Int -> [Color f] -> (Int -> Int -> PixelRGB8)
colorToPixelRGB8 width colors = \worldX worldY -> toPixelRGB8 (colors !! (worldX + worldY * width))

writeParallelPNG :: (NFData f, RealFrac f) => String -> (Int, Int, (Int -> Int -> Color f)) -> IO ()
writeParallelPNG fileName (width, height, generator) =
    let coordinates = [(y, x) | x <- [0..(width - 1)], y <- [0..(height - 1)]]
    in do colors <- evaluate $ force $ (map (generatorTuple generator) coordinates `using` parListChunk 32 rseq)
          let !pixelRGB8Generator = colors `pseq` colorToPixelRGB8 width colors
          savePngImage fileName (ImageRGB8 (generateImage pixelRGB8Generator width height))
