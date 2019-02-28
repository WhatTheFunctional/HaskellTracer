module Scene
    ( Scene (..)
    , transformScene
    ) where

import Linear
import Linear.Matrix

import Object

data Scene f = ListScene [Object f] deriving (Show)

transformScene :: (Fractional f) => M44 f -> M44 f -> Scene f -> Scene f
transformScene worldToView normalMatrix (ListScene objects) = ListScene (fmap (transformObject worldToView normalMatrix) objects)

