module Screen
    ( pixelTraceGenerator
    ) where

import System.Random
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
import Sampling

pixelTraceGenerator :: (RealFloat f, RandomGen g, Integral i, Random i, LowDiscrepancySequence s i f)
                    => (Scene f -> Color f -> Ray f -> s i f -> ((TraceResult f, Ray f), s i f))
                    -> ((Ray f -> s i f -> ((TraceResult f, Ray f), s i f)) -> [Light f] -> Color f -> (TraceResult f, Ray f) -> s i f -> (Color f, s i f))
                    -> Scene f
                    -> [Light f]
                    -> Color f
                    -> Camera f
                    -> (i, i, f, f)
                    -> (f -> f -> f -> f -> f -> s i f -> ([Ray f], s i f))
                    -> (i -> s i f)
                    -> g
                    -> (Int, Int, (Int -> Int -> Color f))
pixelTraceGenerator traceFunction lightingFunction scene lights bgColor camera (width, height, pixelSize, gamma) samplingFunction mkGen rgen0 =
    let invGamma = 1 / gamma
        w = fromIntegral width
        h = fromIntegral height
        (randomOffset, rgen3) = foldr (\x (accumulator, rgen1) ->
                                           let (r, rgen2) = randomR (0, (width * height)) rgen1
                                           in ((fromIntegral r) : accumulator, rgen2)) ([], rgen0) [0..(width * height - 1)]
    in (fromIntegral width,
        fromIntegral height,
        (\pixelX pixelY -> let index = pixelX + pixelY * (fromIntegral width)
                               gen = mkGen (randomOffset !! index)
                               worldX = ((fromIntegral pixelX) - (w / 2) + 0.5) * pixelSize
                               worldY = ((h / 2) - (fromIntegral pixelY) + 0.5) * pixelSize
                               (CameraTransforms {w2v = worldToView, nM = normalMatrix}) = computeCameraTransforms camera
                               transformedScene = transformScene worldToView normalMatrix scene
                               transformedLights = fmap (transformLight worldToView normalMatrix) lights
                               innerTraceFunction = traceFunction transformedScene bgColor
                               innerLightingFunction = lightingFunction innerTraceFunction transformedLights bgColor
                               (rays, gen0) = samplingFunction pixelSize w h worldX worldY gen
                               (colorSum, gen4) = foldr (\ray (accumulatedColor, gen1) ->
                                                             let ((rayTraceResult, newRay), gen2) = innerTraceFunction ray gen1
                                                                 (lightTraceResult, gen3) = innerLightingFunction (rayTraceResult, newRay) gen2
                                                             in (lightTraceResult ^+^ accumulatedColor, gen3)) ((pure 0), gen0) rays
                           in fmap (\x -> x ** invGamma) (colorSum ^/ fromIntegral (length rays))))

