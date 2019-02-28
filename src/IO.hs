module IO
    ( writePNG
    ) where

import Codec.Picture
import Linear

import Color

toPixelRGB8 :: (RealFrac f) => Color f -> PixelRGB8
toPixelRGB8 (RGB r g b) =
    PixelRGB8 (fromIntegral (round (r * 255.0))) (fromIntegral (round (g * 255.0))) (fromIntegral (round (b * 255.0)))

generatorToPixelRGB8 :: (RealFrac f) => (Int -> Int -> Color f) -> (Int -> Int -> PixelRGB8)
generatorToPixelRGB8 generator = \worldX worldY -> toPixelRGB8 (generator worldX worldY)

writePNG :: (RealFrac f) => String -> (Int, Int, (Int -> Int -> Color f)) -> IO ()
writePNG fileName generator =
    let (width, height, generatorFunction) = generator
        pixelRGB8Generator = generatorToPixelRGB8 generatorFunction
    in savePngImage fileName (ImageRGB8 (generateImage pixelRGB8Generator width height))

