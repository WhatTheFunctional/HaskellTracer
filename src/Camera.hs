module Camera
    ( Camera (..)
    , CameraTransforms (..)
    , computeCameraTransforms
    ) where

import Linear
import Linear.Affine
import Linear.Vector

data Camera f = Camera (Point V3 f) (V3 f) (V3 f) -- Origin, look, and up
              deriving (Show, Eq)

-- View to world and world to view
data CameraTransforms f = CameraTransforms { w2v :: M44 f
                                           , normalMatrix :: M44 f
                                           } deriving (Show, Eq)

worldToView :: (Floating f) => Camera f -> M44 f
worldToView (Camera cameraOrigin cameraLook cameraUp) =
    let cr = (cameraUp `cross` cameraLook)
        cu = cameraUp
        cl = cameraLook
        co = unP cameraOrigin
        translation = V4 (-(cr `dot` co)) (-(cu `dot` co)) (-(cl `dot` co)) 1.0
    in V4 (vector cr) (vector cu) (vector cl) translation -- Column major

computeCameraTransforms :: (Floating f) => Camera f -> CameraTransforms f
computeCameraTransforms camera =
    let wV = transpose (worldToView camera) -- Column major
        invWV = transpose (inv44 wV) -- Column major
    in (CameraTransforms {w2v = wV, normalMatrix = invWV})

