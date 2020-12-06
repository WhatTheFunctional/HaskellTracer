module Main where

import Control.DeepSeq
import Data.List
import Numeric.Limits
import System.IO
import System.Random
import System.Random.Shuffle
import Linear
import Linear.Affine

import Color
import Ray
import Geometry
import Object
import Trace
import Lens
import Sampling
import Camera
import Screen
import Scene
import IO
import Light
import Material
import Shading
import Accelerator
import Mesh

-- Bunny light scene

testSkyBlueRGB :: Color Double
testSkyBlueRGB = RGB 0.529 0.808 0.922

bunnyLight0 :: Light
bunnyLight0 = RectangleLight (P (V3 300 300 300)) (V3 0 50 0) (V3 0 0 50) (RGB 1 1 1)

bunnyPlane :: Object
bunnyPlane = Object (Plane (P (V3 0 50 0)) (V3 0 1 0)) (MatteMaterial (RGB 1 1 1) 1) lambertShader

bunnyCamera :: Camera
bunnyCamera = cameraLookAt (P (V3 20 300 400)) (V3 0 150 0) (V3 0 1 0)

testRenderBunnyScene :: [Object] -> IO ()
testRenderBunnyScene objects =
    do putStrLn "-- Writing bunny scene to bunny_scene.png"
       hFlush stdout
       let gen0 = mkStdGen 813580
           (cache, count) = mkHaltonCache 1048576 2
           cache' = shuffle' cache 1048576 gen0
       writeParallelPNG "bunny_scene.png"
                (pixelTraceGenerator
                 traceRays
                 traceOneLight
                 (KDScene (buildKDTree defaultTi defaultTt defaultEmptyBonus standardMaxDepth (bunnyPlane : objects)))
                 [bunnyLight0]
                 testSkyBlueRGB
                 bunnyCamera
                 (1024, 768, 0.195, 2.2)
                 (randomSampling 32 (perspectiveLens (pi / 12)))
                 (mkHaltonLDS (cache', count))
                 gen0)

runJustRenderBunny :: IO ()
runJustRenderBunny =
    do let s = 2000
           transform = (V4 (V4 s 0 0 0)
                           (V4 0 s 0 0)
                           (V4 0 0 s 0)
                           (V4 0 0 0 1))
       mesh <- loadMeshPLY transform
                           (PlasticMaterial (RGB 0.2 0.2 0.5) 1 (RGB 0.4 0.4 0.8) 1 2.5)
                           lambertShader
                           "bun_zipper_res3.ply"
       case mesh of
           Nothing -> return ()
           Just (plyHeader, objects) ->
               do putStrLn $ show $ plyHeader
                  putStrLn $ ("Mesh size: " ++ (show $ length objects))
                  hFlush stdout
                  testRenderBunnyScene objects

main :: IO ()
main = do
    putStrLn "Run HaskellTrace"
    hFlush stdout
    runJustRenderBunny
