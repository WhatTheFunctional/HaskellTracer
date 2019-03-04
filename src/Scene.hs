module Scene
    ( Scene (..)
    , transformScene
    ) where

import Linear
import Linear.Matrix

import Geometry
import Object
import Accelerator

data Scene f = ListScene [Object f]
             | KDScene (KDTree f)
             deriving (Show)

-- KD node transformation

transformKDNode :: (Fractional f) => M44 f -> M44 f -> KDNode f -> KDNode f
transformKDNode worldToView normalMatrix (KDLeaf objects) = KDLeaf (fmap (transformObject worldToView normalMatrix) objects)
transformKDNode worldToView normalMatrix (KDBranch v left right) = KDBranch v (transformKDNode worldToView normalMatrix left) (transformKDNode worldToView normalMatrix right)

-- Scene transformation

transformScene :: (Fractional f) => M44 f -> M44 f -> Scene f -> Scene f
transformScene worldToView normalMatrix (ListScene objects) = ListScene (fmap (transformObject worldToView normalMatrix) objects)
transformScene worldToView normalMatrix (KDScene (KDTree aabb planes node)) =
    KDScene (KDTree (transformShape worldToView normalMatrix aabb) (fmap (transformObject worldToView normalMatrix) planes) (transformKDNode worldToView normalMatrix node))

