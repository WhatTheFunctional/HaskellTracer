module Object
    ( Object (..)
    ) where

import Geometry
import Color

data Object f c = ColorObject (Shape f) c

