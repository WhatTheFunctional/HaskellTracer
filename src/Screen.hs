module Screen
    ( pixelTraceGenerator
    ) where

import System.Random
import Linear
import Data.List

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

pixelTraceGenerator :: (RandomGen g, LowDiscrepancySequence s)
                    => (Scene
                        -> Color Double
                        -> Ray
                        -> s
                        -> ((TraceResult, Ray), s))
                    -> ((Ray
                         -> s
                         -> ((TraceResult, Ray), s))
                         -> [Light]
                         -> Color Double
                         -> (TraceResult, Ray)
                         -> s
                         -> (Color Double, s))
                    -> Scene
                    -> [Light]
                    -> Color Double
                    -> Camera
                    -> (Int, Int, Double, Double)
                    -> (Double
                        -> Double
                        -> Double
                        -> Double
                        -> Double
                        -> s
                        -> ([Ray], s))
                    -> (Int -> s)
                    -> g
                    -> (Int, Int, (Int
                                   -> Int
                                   -> Color Double))

pixelTraceGenerator traceFunction lightingFunction scene lights bgColor camera (width, height, pixelSize, gamma) samplingFunction mkGen rgen0 =
    let invGamma = 1 / gamma
        w = fromIntegral width
        h = fromIntegral height
        (randomOffset, rgen3) = foldl' (\(accumulator, rgen1) x ->
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
                               (colorSum, gen4) = foldl' (\(accumulatedColor, gen1) ray ->
                                                              let ((rayTraceResult, newRay), gen2) = innerTraceFunction ray gen1
                                                                  (lightTraceResult, gen3) = innerLightingFunction (rayTraceResult, newRay) gen2
                                                              in (lightTraceResult ^+^ accumulatedColor, gen3)) ((pure 0), gen0) rays
                           in fmap (\x -> x ** invGamma) (colorSum ^/ fromIntegral (length rays))))

