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

pixelTraceGenerator :: (Color c, Epsilon f, Floating f, Ord f, RealFloat f, Integral i) => (M44 f -> M44 f -> c -> Ray f -> c) -> c -> Camera f -> ViewPlane f i -> (f -> f -> [Ray f]) -> (Int, Int, (Int -> Int -> c))
pixelTraceGenerator traceFunction bgColor camera (ViewPlane {width = w, height = h, pixelSize = s, gamma = g, invGamma = ig}) lensFunction =
    (fromIntegral w,
     fromIntegral h,
     (\pixelX pixelY -> let worldX = ((fromIntegral pixelX) - ((fromIntegral w) / 2.0) + 0.5) * s
                            worldY = (((fromIntegral h) / 2.0) - (fromIntegral pixelY) + 0.5) * s
                            (CameraTransforms {w2v = worldToView, normalMatrix = nMatrix}) = computeCameraTransforms camera
                            rays = lensFunction worldX worldY
                            --transformedRays = fmap (transformRay worldToView nMatrix) rays
                        in traceRays (traceFunction worldToView nMatrix bgColor) rays blankColor))

