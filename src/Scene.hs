module Scene
    ( ListScene (..)
    ) where

import Object

data ListScene f c = ListScene [Object f c] deriving (Show, Eq)

