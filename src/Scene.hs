module Scene
    ( ListScene (..)
    ) where

import Object

data ListScene f = ListScene [Object f] deriving (Show)

