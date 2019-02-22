module Color
    ( Color (..)
    , RGB (..)
    ) where

import Linear

class Color a where
    backgroundColor :: a

data RGB f = RGB (V3 f)

instance (Floating f) => Color (RGB f) where
    backgroundColor = RGB (V3 0.0 0.0 0.0)
