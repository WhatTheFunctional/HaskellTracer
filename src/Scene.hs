module Scene
    ( Scene (..)
    , transformScene
    ) where

import Linear
import Linear.Matrix

import Geometry
import Object
import Accelerator

data Scene = ListScene [Object]
           | KDScene KDTree
           deriving (Show)

-- KD node transformation

transformKDNode :: M44 Double -> M44 Double -> KDNode -> KDNode
transformKDNode worldToView normalMatrix (KDLeaf objects) = KDLeaf (fmap (transformObject worldToView normalMatrix) objects)
transformKDNode worldToView normalMatrix (KDBranch v left right) = KDBranch v (transformKDNode worldToView normalMatrix left) (transformKDNode worldToView normalMatrix right)

-- Scene transformation

transformScene :: M44 Double -> M44 Double -> Scene -> Scene
transformScene worldToView normalMatrix (ListScene objects) = ListScene (fmap (transformObject worldToView normalMatrix) objects)
transformScene worldToView normalMatrix (KDScene (KDTree aabb planes node)) =
    KDScene (KDTree (transformShape worldToView normalMatrix aabb) (fmap (transformObject worldToView normalMatrix) planes) (transformKDNode worldToView normalMatrix node))

