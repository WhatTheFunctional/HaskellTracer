module Color
    ( Color (..)
    , RGB (..)
    , makeRGB
    ) where

import Linear

class Color a where
    blankColor :: a
    backgroundColor :: a
    mixColors :: a -> a -> a

data RGB f = RGB (V3 f) f -- Triple + weight
           deriving (Show, Eq)

makeRGB :: (RealFloat f, Floating f) => f -> f -> f -> RGB f
makeRGB r g b = RGB (V3 r g b) 1.0

instance (RealFloat f, Floating f) => Color (RGB f) where
    blankColor = RGB (V3 0.0 0.0 0.0) 0.0
    backgroundColor = RGB (V3 0.0 0.0 0.0) 1.0
    mixColors (RGB (V3 r0 g0 b0) w0) (RGB (V3 r1 g1 b1) w1) =
        let mixWeight = w0 + w1
        in (RGB (V3 ((r0 * w0 + r1 * w1) / mixWeight) ((g0 * w0 + g1 * w1) / mixWeight) ((b0 * w0 + b1 * w1) / mixWeight)) mixWeight)

