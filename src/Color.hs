module Color
    ( Color (..)
    , RGB (..)
    , makeRGB
    ) where

import Codec.Picture
import Linear

class Color a where
    blackColor :: a
    backgroundColor :: a
    mixColors :: a -> a -> a
    toPixelRGB8 :: a -> PixelRGB8

data RGB f = RGB (V3 f) f -- Triple + weight
           deriving (Show, Eq)

makeRGB :: (RealFrac f, Floating f) => f -> f -> f -> RGB f
makeRGB r g b = RGB (V3 r g b) 1.0

instance (RealFrac f, Floating f) => Color (RGB f) where
    blackColor = RGB (V3 0.0 0.0 0.0) 1.0
    backgroundColor = RGB (V3 0.0 0.0 0.0) 1.0
    mixColors (RGB (V3 r0 g0 b0) w0) (RGB (V3 r1 g1 b1) w1) =
        let mixWeight = w0 + w1
        in (RGB (V3 ((r0 * w0 + r1 * w1) / mixWeight) ((g0 * w0 + g1 * w1) / mixWeight) ((b0 * w0 + b1 * w1) / mixWeight)) mixWeight)
    toPixelRGB8 (RGB (V3 r g b) _) =
        PixelRGB8 (fromIntegral (round (r * 255.0))) (fromIntegral (round (g * 255.0))) (fromIntegral (round (b * 255.0)))

