module Camera
    ( Camera (..)
    , CameraTransforms (..)
    , cameraLookAt
    , computeCameraTransforms
    ) where

import Linear
import Linear.Affine
import Linear.Vector

data Camera f = Camera (Point V3 f) (V3 f) (V3 f) -- Origin, look, and up
              deriving (Show, Eq)

data CameraTransforms f = CameraTransforms { w2v :: M44 f -- World to view
                                           , nM :: M44 f -- Normal matrix
                                           }
                        deriving (Show, Eq)

cameraLookAt :: (Epsilon f, Floating f) => Point V3 f -> V3 f -> V3 f -> Camera f
cameraLookAt cameraOrigin cameraPoint cameraUp =
    let cl = normalize (cameraPoint ^-^ (unP cameraOrigin))
        cr = cameraUp `cross` cl
        cu = cl `cross` cr
    in Camera cameraOrigin cl cu

getWorldMatrix :: (Num f) => Camera f -> M44 f
getWorldMatrix (Camera cameraOrigin cameraLook cameraUp) =
    let cr = (cameraUp `cross` cameraLook)
        cu = cameraUp
        cl = cameraLook
        co = unP cameraOrigin
        translation = V4 (-(cr `dot` co)) (-(cu `dot` co)) (-(cl `dot` co)) 1
    in V4 (vector cr) (vector cu) (vector cl) translation -- Column major

computeCameraTransforms :: (Fractional f) => Camera f -> CameraTransforms f
computeCameraTransforms camera =
    let wV = transpose (getWorldMatrix camera) -- Column major
        invWV = transpose (inv44 wV) -- Column major
    in (CameraTransforms {w2v = wV, nM = invWV})

