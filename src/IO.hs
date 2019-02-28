module IO
    ( writePNG
    ) where

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
writePNG fileName generator =
    let (width, height, generatorFunction) = generator
        pixelRGB8Generator = generatorToPixelRGB8 generatorFunction
    in savePngImage fileName (ImageRGB8 (generateImage pixelRGB8Generator width height))

