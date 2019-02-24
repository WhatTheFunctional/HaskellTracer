module Screen
    ( ViewPlane (..)
    , pixelTraceGenerator
    ) where

import Linear

import Color
import Camera
import Ray
import Object
import Trace
import Scene

data ViewPlane f i = ViewPlane { width :: i
                               , height :: i
                               , pixelSize :: f
                               , gamma :: f
                               , invGamma :: f
                               }

pixelTraceGenerator :: (Epsilon f, Ord f, Floating f, RealFloat f, Integral i, Color c) => (M44 f -> M44 f -> c -> Ray f -> c) -> c -> Camera f -> ViewPlane f i -> (f -> f -> [Ray f]) -> (Int -> Int -> c)
pixelTraceGenerator traceFunction bgColor camera (ViewPlane {width = w, height = h, pixelSize = s, gamma = g, invGamma = ig}) lensFunction =
    (\pixelX pixelY -> let worldX = (((fromIntegral w) / 2.0) - (fromIntegral pixelX) + 0.5) * s
                           worldY = ((fromIntegral pixelY) - ((fromIntegral h) / 2.0) + 0.5) * s
                           rays = lensFunction worldX worldY
                           (CameraTransforms {v2w = viewToWorld, w2v = worldToView}) = computeCameraTransforms camera
                       in traceRays (traceFunction viewToWorld worldToView bgColor) rays blankColor)

