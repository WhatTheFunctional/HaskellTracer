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

clampComponent :: Double -> Int
clampComponent x = round (max 0.0 (min 255.0 (x * 255)))

toPixelRGB8 :: Color Double -> PixelRGB8
toPixelRGB8 (RGB r g b) =
    PixelRGB8 (fromIntegral (clampComponent r))
              (fromIntegral (clampComponent g))
              (fromIntegral (clampComponent b))

generatorToPixelRGB8 :: (Int -> Int -> Color Double) -> (Int -> Int -> PixelRGB8)
generatorToPixelRGB8 generator = \worldX worldY -> toPixelRGB8 (generator worldX worldY)

writePNG :: String -> (Int, Int, (Int -> Int -> Color Double)) -> IO ()
writePNG fileName (width, height, generator) =
    let pixelRGB8Generator = generatorToPixelRGB8 generator
    in savePngImage fileName (ImageRGB8 (generateImage pixelRGB8Generator width height))

generatorTuple :: (Int -> Int -> Color Double) -> (Int, Int) -> Color Double
generatorTuple generator (x, y) = generator x y

colorToPixelRGB8 :: Int -> [Color Double] -> (Int -> Int -> PixelRGB8)
colorToPixelRGB8 height colors = \worldX worldY -> toPixelRGB8 (colors !! (worldY + worldX * height))

writeParallelPNG :: String -> (Int, Int, (Int -> Int -> Color Double)) -> IO ()
writeParallelPNG fileName (width, height, generator) =
    let coordinates = [(x, y) | x <- [0..(width - 1)], y <- [0..(height - 1)]]
    in do colors <- evaluate $ force $ (map (generatorTuple generator) coordinates `using` parListChunk 32 rseq)
          let pixelRGB8Generator = colorToPixelRGB8 height colors
          savePngImage fileName (ImageRGB8 (generateImage pixelRGB8Generator width height))

