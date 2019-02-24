module Screen
    ( pixelTraceGenerator
    ) where

import Linear

import Color
import Camera
import Ray
import Object
import Trace
import Scene

pixelTraceGenerator :: (Color c, Epsilon f, Floating f, Ord f, RealFloat f, Integral i) => (M44 f -> M44 f -> c -> Ray f -> c) -> c -> Camera f -> (i, i, f, f) -> (f -> f -> f -> [Ray f]) -> (Int, Int, (Int -> Int -> c))
pixelTraceGenerator traceFunction bgColor camera (width, height, pixelSize, gamma) samplingFunction =
    (fromIntegral width,
     fromIntegral height,
     (\pixelX pixelY -> let worldX = ((fromIntegral pixelX) - ((fromIntegral width) / 2.0) + 0.5) * pixelSize
                            worldY = (((fromIntegral height) / 2.0) - (fromIntegral pixelY) + 0.5) * pixelSize
                            (CameraTransforms {w2v = worldToView, normalMatrix = nMatrix}) = computeCameraTransforms camera
                            rays = samplingFunction pixelSize worldX worldY
                        in traceRays (traceFunction worldToView nMatrix bgColor) rays blankColor))

