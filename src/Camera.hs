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

data CameraTransforms f = CameraTransforms { w2v :: M44 f -- World to view
                                           , nM :: M44 f -- Normal matrix
                                           } deriving (Show, Eq)

worldToView :: (Num f) => Camera f -> M44 f
worldToView (Camera cameraOrigin cameraLook cameraUp) =
    let cr = (cameraUp `cross` cameraLook)
        cu = cameraUp
        cl = cameraLook
        co = unP cameraOrigin
        translation = V4 (-(cr `dot` co)) (-(cu `dot` co)) (-(cl `dot` co)) 1
    in V4 (vector cr) (vector cu) (vector cl) translation -- Column major

computeCameraTransforms :: (Fractional f, Num f) => Camera f -> CameraTransforms f
computeCameraTransforms camera =
    let wV = transpose (worldToView camera) -- Column major
        invWV = transpose (inv44 wV) -- Column major
    in (CameraTransforms {w2v = wV, nM = invWV})

