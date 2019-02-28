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

pixelTraceGenerator :: (Floating f, Integral i) =>
                       (Scene f -> Ray f -> (Intersection f, Material f, (V3 f -> Material f -> V3 f -> V3 f -> Color f))) ->
                       (Scene f -> [Light f] -> Color f -> Ray f -> (Intersection f, Material f, (V3 f -> Material f -> V3 f -> V3 f -> Color f)) -> Color f) ->
                       Scene f -> [Light f] -> Color f -> Camera f -> (i, i, f, f) -> (f -> f -> f -> [Ray f]) -> (Int, Int, (Int -> Int -> Color f))
pixelTraceGenerator traceFunction lightingFunction scene lights bgColor camera (width, height, pixelSize, gamma) samplingFunction =
    (fromIntegral width,
     fromIntegral height,
     (\pixelX pixelY -> let worldX = ((fromIntegral pixelX) - ((fromIntegral width) / 2) + 0.5) * pixelSize
                            worldY = (((fromIntegral height) / 2) - (fromIntegral pixelY) + 0.5) * pixelSize
                            (CameraTransforms {w2v = worldToView, nM = normalMatrix}) = computeCameraTransforms camera
                            transformedScene = transformScene worldToView normalMatrix scene
                            transformedLights = fmap (transformLight worldToView normalMatrix) lights
                            rays = samplingFunction pixelSize worldX worldY
                            innerTraceFunction = traceFunction transformedScene
                            innerLightingFunction = lightingFunction transformedScene transformedLights bgColor
                        in (foldr (\ray accumulatedColor -> (innerLightingFunction ray (innerTraceFunction ray)) ^+^ accumulatedColor) (pure 0) rays) ^/ fromIntegral (length rays)))

