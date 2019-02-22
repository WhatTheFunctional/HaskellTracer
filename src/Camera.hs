module Camera
    ( ViewPlane (..)
    , Camera (..)
    ) where

import Linear
import Linear.Affine

data ViewPlane f i = ViewPlane { width :: i
                               , height :: i
                               , pixelSize :: f
                               , gamma :: f
                               , invGamma :: f
                               }

data Camera f = Camera (Point V3 f) (V3 f) (V3 f) -- Origin, look, and up
              deriving (Show, Eq)

-- View to world and world to view
data CameraTransforms f = CameraTransforms { v2w :: M44 f
                                           , w2v :: M44 f
                                           } deriving (Show, Eq)

viewToWorld :: (Floating f) => Camera f -> M44 f
viewToWorld (Camera cameraOrigin cameraLook cameraUp) =
    let cr = vector (cameraLook `cross` cameraUp)
        cu = vector cameraUp
        cl = vector cameraLook
        co = point (unP cameraOrigin)
    in V4 cr cu cl co

computeCameraTransforms :: (Floating f) => Camera f -> CameraTransforms f
computeCameraTransforms camera =
    let vW = viewToWorld camera
        invVW = inv44 vW
    in (CameraTransforms {v2w = vW, w2v = invVW})
