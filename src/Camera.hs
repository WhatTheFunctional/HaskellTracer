module Camera
    ( Camera (..)
    , CameraTransforms (..)
    , cameraLookAt
    , computeCameraTransforms
    ) where

import Linear
import Linear.Affine
import Linear.Vector

data Camera = Camera (Point V3 Double) (V3 Double) (V3 Double) -- Origin, look, and up
              deriving (Show, Eq)

data CameraTransforms = CameraTransforms { w2v :: M44 Double -- World to view
                                         , nM :: M44 Double -- Normal matrix
                                         }
                        deriving (Show, Eq)

cameraLookAt :: Point V3 Double -> V3 Double -> V3 Double -> Camera
cameraLookAt cameraOrigin cameraPoint cameraUp =
    let cl = normalize (cameraPoint ^-^ (unP cameraOrigin))
        cr = cameraUp `cross` cl
        cu = cl `cross` cr
    in Camera cameraOrigin cl cu

getWorldMatrix :: Camera -> M44 Double
getWorldMatrix (Camera cameraOrigin cameraLook cameraUp) =
    let cr = (cameraUp `cross` cameraLook)
        cu = cameraUp
        cl = cameraLook
        co = unP cameraOrigin
        translation = V4 (-(cr `dot` co)) (-(cu `dot` co)) (-(cl `dot` co)) 1
    in V4 (vector cr) (vector cu) (vector cl) translation -- Column major

computeCameraTransforms :: Camera -> CameraTransforms
computeCameraTransforms camera =
    let wV = transpose (getWorldMatrix camera) -- Column major
        invWV = transpose (inv44 wV) -- Column major
    in (CameraTransforms {w2v = wV, nM = invWV})

