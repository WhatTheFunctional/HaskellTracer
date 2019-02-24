module Screen
    ( ViewPlane (..)
    , listTraceGenerator
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

listTraceGenerator :: (Epsilon f, Ord f, Floating f, RealFloat f, Integral i, Color c) => c -> Camera f -> ViewPlane f i -> [Object f c] -> (f -> f -> [Ray f]) -> (Int -> Int -> c)
listTraceGenerator bgColor camera (ViewPlane {width = w, height = h, pixelSize = s, gamma = g, invGamma = ig}) objects lensFunction =
    (\pixelX pixelY -> let worldX = (((fromIntegral w) / 2.0) - (fromIntegral pixelX) + 0.5) * s
                           worldY = ((fromIntegral pixelY) - ((fromIntegral h) / 2.0) + 0.5) * s
                           rays = lensFunction worldX worldY
                           (CameraTransforms {v2w = viewToWorld, w2v = worldToView}) = computeCameraTransforms camera
                           transformedObjects = fmap (transformObject viewToWorld worldToView) objects
                       in traceRays (listTrace bgColor (ListScene transformedObjects)) rays blankColor)

