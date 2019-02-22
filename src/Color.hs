module Color
    ( Color (..)
    , RGB (..)
    ) where

import Codec.Picture
import Linear

class Color a where
    backgroundColor :: a
    toPixelRGB8 :: a -> PixelRGB8

data RGB f = RGB (V3 f) deriving (Show, Eq)

instance (RealFrac f, Floating f) => Color (RGB f) where
    backgroundColor = RGB (V3 0.0 0.0 0.0)
    toPixelRGB8 (RGB (V3 r g b)) =
        PixelRGB8 (fromIntegral (round (r * 255.0))) (fromIntegral (round (g * 255.0))) (fromIntegral (round (b * 255.0)))

