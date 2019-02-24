module IO
    ( writePNG
    ) where

import Codec.Picture
import Linear

import Color

class ConvertsToPixelRGB8 a where
    toPixelRGB8 :: a -> PixelRGB8

instance (RealFrac f, Floating f) => ConvertsToPixelRGB8 (RGB f) where
    toPixelRGB8 (RGB (V3 r g b) _) =
        PixelRGB8 (fromIntegral (round (r * 255.0))) (fromIntegral (round (g * 255.0))) (fromIntegral (round (b * 255.0)))

generatorToPixelRGB8 :: (ConvertsToPixelRGB8 c, Color c) => (Int -> Int -> c) -> (Int -> Int -> PixelRGB8)
generatorToPixelRGB8 generator = \worldX worldY -> toPixelRGB8 (generator worldX worldY)

writePNG :: (ConvertsToPixelRGB8 c, Color c) => String -> (Int, Int, (Int -> Int -> c)) -> IO ()
writePNG fileName generator =
    let (width, height, generatorFunction) = generator
        pixelRGB8Generator = generatorToPixelRGB8 generatorFunction
    in savePngImage fileName (ImageRGB8 (generateImage pixelRGB8Generator width height))

