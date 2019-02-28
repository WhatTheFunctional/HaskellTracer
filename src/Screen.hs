module Screen
    ( pixelTraceGenerator
    ) where

import Linear

import Camera
import Ray
import Object
import Trace
import Scene
import Light
import Color
import Material

pixelTraceGenerator :: (Fractional f, Integral i) => (s -> M44 f -> M44 f -> Color f -> Ray f -> (Intersection f, Material f, (V3 f -> Material f -> V3 f -> V3 f -> Color f))) ->
                       (s -> [Light f] -> M44 f -> M44 f -> Color f -> Ray f -> (Intersection f, Material f, (V3 f -> Material f -> V3 f -> V3 f -> Color f)) -> Color f) ->
                       s -> [Light f] -> Color f -> Camera f -> (i, i, f, f) -> (f -> f -> f -> [Ray f]) -> (Int, Int, (Int -> Int -> Color f))
pixelTraceGenerator traceFunction lightingFunction scene lights bgColor camera (width, height, pixelSize, gamma) samplingFunction =
    (fromIntegral width,
     fromIntegral height,
     (\pixelX pixelY -> let worldX = ((fromIntegral pixelX) - ((fromIntegral width) / 2) + 0.5) * pixelSize
                            worldY = (((fromIntegral height) / 2) - (fromIntegral pixelY) + 0.5) * pixelSize
                            (CameraTransforms {w2v = worldToView, normalMatrix = nMatrix}) = computeCameraTransforms camera
                            rays = samplingFunction pixelSize worldX worldY
                            innerTraceFunction = traceFunction scene worldToView nMatrix bgColor
                            innerLightingFunction = lightingFunction scene lights worldToView nMatrix bgColor
                        in (foldr (\ray accumulatedColor -> (innerLightingFunction ray (innerTraceFunction ray)) ^+^ accumulatedColor) (pure 0) rays) ^/ fromIntegral (length rays)))

