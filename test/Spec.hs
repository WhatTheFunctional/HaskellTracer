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

-- Test objects

testSphere :: (Num f) => Shape f
testSphere = Sphere (P (V3 0 0 100)) 100

testPlane :: (Num f) => Shape f
testPlane = Plane (P (V3 0 0 10)) (V3 0 1 1)

testMissPlane :: (Num f) => Shape f
testMissPlane = Plane (P (V3 0 (-10) 0)) (V3 0 1 0)

testHitRay :: (Num f) => Ray f
testHitRay = Ray (P (V3 0 0 0)) (V3 0 0 1)

testMissRay :: (Num f) => Ray f
testMissRay = Ray (P (V3 0 1000 0)) (V3 0 0 1)

testBlackRGB :: (Num f) => Color f
testBlackRGB = RGB 0 0 0

testRedRGB :: (Num f) => Color f
testRedRGB = RGB 1 0 0

testGreenRGB :: (Num f) => Color f
testGreenRGB = RGB 0 1 0

testPinkRGB :: (Num f) => Color f
testPinkRGB = RGB 1 0 1

testCamera :: (Num f) => Camera f
testCamera = Camera (P (V3 0 0 0)) (V3 0 0 1) (V3 0 1 0)

-- Test scene

suffernCamera :: (Num f) => Camera f
suffernCamera = Camera (P (V3 0 0 100)) (V3 0 0 (-1)) (V3 0 1 0)

suffernSphere0 :: (Num f) => Object f
suffernSphere0 = Object (Sphere (P (V3 0 (-25) 0)) 80) (ColorMaterial (RGB 1 0 0)) colorShader

suffernSphere1 :: (Num f) => Object f
suffernSphere1 = Object (Sphere (P (V3 0 30 0)) 60) (ColorMaterial (RGB 1 1 0)) colorShader

suffernPlane :: (Fractional f) => Object f
suffernPlane = Object (Plane (P (V3 0 0 0)) (V3 0 1 1)) (ColorMaterial (RGB 0 0.3 0)) colorShader

-- Test functions

testRayIntersectSphere :: IO ()
testRayIntersectSphere = do
    putStrLn "-- Testing Ray Sphere Intersection"
    putStrLn $ show $ rayIntersection (testHitRay :: Ray Float) testSphere

testRayIntersectPlane :: IO ()
testRayIntersectPlane = do
    putStrLn "-- Testing Ray Plane Intersection"
    putStrLn $ show $ rayIntersection (testHitRay :: Ray Float) testPlane

testRayMissSphere :: IO ()
testRayMissSphere = do
    putStrLn "-- Testing Ray Sphere Miss"
    putStrLn $ show $ rayIntersection (testMissRay :: Ray Float) testSphere

testRayMissPlane :: IO ()
testRayMissPlane = do
    putStrLn "-- Testing Ray Plane Miss"
    putStrLn $ show $ rayIntersection (testHitRay :: Ray Float) testMissPlane

testNaiveTraceSphere :: IO ()
testNaiveTraceSphere = do
    putStrLn "-- Testing Naive Trace with a Sphere"
    putStrLn $ show $ listTrace (ListScene [Object testSphere (ColorMaterial (testRedRGB :: Color Float)) colorShader]) [EnvironmentLight testBlackRGB] (identity :: M44 Float) (identity :: M44 Float) (testPinkRGB :: Color Float) (testHitRay :: Ray Float)

testNaiveTracePlane :: IO ()
testNaiveTracePlane = do
    putStrLn "-- Testing Naive Trace with a Sphere"
    putStrLn $ show $ listTrace (ListScene [Object testPlane (ColorMaterial (testRedRGB :: Color Float)) colorShader]) [EnvironmentLight testBlackRGB] (identity :: M44 Float) (identity :: M44 Float) (testPinkRGB :: Color Float) (testHitRay :: Ray Float)

testRenderBasicSphere :: IO ()
testRenderBasicSphere =
    do putStrLn "-- Writing basic sphere image to basic_sphere.png"
       writePNG "basic_sphere.png"
                (pixelTraceGenerator
                 (listTrace (ListScene [Object testSphere (ColorMaterial (testRedRGB :: Color Float)) colorShader]) [EnvironmentLight testBlackRGB])
                 (testPinkRGB :: Color Float)
                 testCamera
                 (1024, 768, 1.0 :: Float, 1.0)
                 (singleSampling orthoLensSingle))

testRenderBasicScene :: IO ()
testRenderBasicScene =
    do putStrLn "-- Writing Suffern scene image to suffern_scene.png"
       writePNG "suffern_scene.png"
                (pixelTraceGenerator
                 (listTrace (ListScene [suffernSphere0, suffernSphere1, suffernPlane]) [EnvironmentLight testBlackRGB])
                 (testPinkRGB :: Color Float)
                 suffernCamera
                 (200, 200, 1.0 :: Float, 1.0)
                 (singleSampling orthoLensSingle))

testRender4xSuperSamplingBasicScene :: IO ()
testRender4xSuperSamplingBasicScene =
    do putStrLn "-- Writing Suffern scene image with 4x supersampling to suffern_scene_4x.png"
       writePNG "suffern_scene_4x.png"
                (pixelTraceGenerator
                 (listTrace (ListScene [suffernSphere0, suffernSphere1, suffernPlane]) [EnvironmentLight testBlackRGB])
                 (testPinkRGB :: Color Float)
                 suffernCamera
                 (200, 200, 1.0 :: Float, 1.0)
                 (grid4xSampling orthoLensSingle))

main :: IO ()
main = do putStrLn "Running tests"
          putStrLn "--Suffern camera"
          putStrLn $ show $ (suffernCamera :: Camera Float)
          let ct@(CameraTransforms {w2v = worldToView, normalMatrix = nM}) = (computeCameraTransforms (suffernCamera :: Camera Float))
              s0@(Object (Sphere (P s0Position) s0Radius) s0Color colorShader) = suffernSphere0
          putStrLn "--Suffern transforms"
          putStrLn $ show $ ct
          putStrLn "--Suffern sphere"
          putStrLn $ show $ s0
          putStrLn "--Suffern sphere view space center"
          putStrLn $ show $ (worldToView !* (point s0Position))
          testRayIntersectSphere
          testRayIntersectPlane
          testRayMissSphere
          testRayMissPlane
          testNaiveTraceSphere
          testNaiveTracePlane
          testRenderBasicSphere
          testRenderBasicScene
          testRender4xSuperSamplingBasicScene
