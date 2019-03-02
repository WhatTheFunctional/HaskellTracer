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
import Shading

invGamma :: (Fractional f) => f
invGamma = 1 / 2.2

pixelTraceGenerator :: (Floating f, Integral i) =>
                       (Scene f -> Color f -> Ray f -> (Intersection f, Material f, (ShadePoint f -> Color f))) ->
                       ((Ray f -> (Intersection f, Material f, (ShadePoint f -> Color f))) -> [Light f] -> Color f -> Ray f -> (Intersection f, Material f, (ShadePoint f -> Color f)) -> Color f) ->
                       Scene f -> [Light f] -> Color f -> Camera f -> (i, i, f, f) -> (f -> f -> f -> f -> f -> [Ray f]) -> (Int, Int, (Int -> Int -> Color f))
pixelTraceGenerator traceFunction lightingFunction scene lights bgColor camera (width, height, pixelSize, gamma) samplingFunction =
    let w = fromIntegral width
        h = fromIntegral height
    in (fromIntegral width,
        fromIntegral height,
        (\pixelX pixelY -> let worldX = ((fromIntegral pixelX) - (w / 2) + 0.5) * pixelSize
                               worldY = ((h / 2) - (fromIntegral pixelY) + 0.5) * pixelSize
                               (CameraTransforms {w2v = worldToView, nM = normalMatrix}) = computeCameraTransforms camera
                               transformedScene = transformScene worldToView normalMatrix scene
                               transformedLights = fmap (transformLight worldToView normalMatrix) lights
                               rays = samplingFunction pixelSize w h worldX worldY
                               innerTraceFunction = traceFunction transformedScene bgColor
                               innerLightingFunction = lightingFunction innerTraceFunction transformedLights bgColor
                           in fmap (\x -> x ** invGamma) ((foldr (\ray accumulatedColor -> (innerLightingFunction ray (innerTraceFunction ray)) ^+^ accumulatedColor) (pure 0) rays) ^/ fromIntegral (length rays))))

