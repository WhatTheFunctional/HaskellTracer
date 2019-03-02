import System.Random
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

testSkyBlueRGB :: (Fractional f) => Color f
testSkyBlueRGB = RGB 0.529 0.808 0.922

testCamera :: (Num f) => Camera f
testCamera = Camera (P (V3 0 0 0)) (V3 0 0 1) (V3 0 1 0)

testEmptyKDTree :: (RealFloat f) => KDTree f
testEmptyKDTree = buildKDTree defaultTi defaultTt defaultEmptyBonus standardMaxDepth []

testKDTree :: (RealFloat f) => KDTree f
testKDTree = buildKDTree defaultTi defaultTt defaultEmptyBonus standardMaxDepth [Object testSphere (ColorMaterial (RGB 1 0 0)) colorShader]

-- Test scene

suffernCamera :: (Epsilon f, Floating f) => Camera f
suffernCamera = Camera (P (V3 0 0 100)) (V3 0 0 (-1)) (V3 0 1 0)

suffernSphere0 :: (Num f) => Object f
suffernSphere0 = Object (Sphere (P (V3 0 (-25) 0)) 80) (ColorMaterial (RGB 1 0 0)) colorShader

suffernSphere1 :: (Num f) => Object f
suffernSphere1 = Object (Sphere (P (V3 0 30 0)) 60) (ColorMaterial (RGB 1 1 0)) colorShader

suffernPlane :: (Fractional f) => Object f
suffernPlane = Object (Plane (P (V3 0 0 0)) (V3 0 1 1)) (ColorMaterial (RGB 0 0.3 0)) colorShader

-- Lit version of Suffern scene

litSuffernCamera :: (Epsilon f, Floating f) => Camera f
litSuffernCamera = cameraLookAt (P (V3 20 50 100)) (V3 0 0 0) (V3 0 1 0)

litSuffernSphere0 :: (Floating f, Ord f) => Object f
litSuffernSphere0 = Object (Sphere (P (V3 0 (-25) 0)) 80) (PlasticMaterial (RGB 1 0 0) 1 (RGB 0.8 0 0.8) 1 2.5) phongShader

litSuffernSphere1 :: (Floating f) => Object f
litSuffernSphere1 = Object (Sphere (P (V3 0 30 0)) 60) (MatteMaterial (RGB 1 1 0) 1) lambertShader

litSuffernPlane :: (Floating f) => Object f
litSuffernPlane = Object (Plane (P (V3 0 0 0)) (V3 0 1 1)) (MatteMaterial (RGB 0 0.3 0) 1) lambertShader

litSuffernAABB :: (Floating f) => Object f
litSuffernAABB = Object (AABB identity (V3 (-65) (-65) (-65)) (V3 65 65 65)) (PlasticMaterial (RGB 0.2 0.2 0.5) 1 (RGB 0.4 0.4 0.8) 1 2.5) lambertShader

litSuffernTriangle :: (Epsilon f, Floating f) => Object f
litSuffernTriangle =
    let v0 = (V3 0 0 100)
        v1 = (V3 100 100 0)
        v2 = (V3 (-100) 0 100)
    in Object (Triangle (P v0) (P v1) (P v2) (normalize ((v1 ^-^ v0) `cross` (v2 ^-^ v0)))) (PlasticMaterial (RGB 0.0 0.0 0.8) 1 (RGB 0.0 0.0 0.1) 1 2.5) lambertShader

litSuffernDisk :: (Floating f) => Object f
litSuffernDisk = Object (Disk (P (V3 0 100 0)) (V3 0 1 0) 40) (PlasticMaterial (RGB 0.0 0.8 0.8) 1 (RGB 0.0 1.0 1.0) 1 2.5) lambertShader

litSuffernRectangle :: (Floating f) => Object f
litSuffernRectangle = Object (Rectangle (P (V3 (-25) 120 (-25))) (V3 50 0 0) (V3 0 0 50) (V3 0 1 0)) (PlasticMaterial (RGB 0.8 0.0 0.8) 1 (RGB 1.0 0.0 1.0) 1 2.5) lambertShader

suffernLight0 :: (Num f) => Light f
suffernLight0 = PointLight (P (V3 80 80 100)) (RGB 1 1 1)

suffernLight1 :: (Num f) => Light f
suffernLight1 = PointLight (P (V3 0 (-40) 100)) (RGB 1 0 1)

suffernLight2 :: (Fractional f) => Light f
suffernLight2 = DirectionalLight (V3 1 (-1) (-1)) (RGB 0.5 0.5 0)

suffernLight3 :: (Fractional f) => Light f
suffernLight3 = DirectionalLight (V3 0 (-1) 0) (RGB 1 1 1)

-- Random spheres scene

randomSpheresCamera :: (Epsilon f, Floating f) => Camera f
randomSpheresCamera = cameraLookAt (P (V3 20 200 400)) (V3 0 150 0) (V3 0 1 0)

randomSpheres :: (Floating f, Random f, RandomGen g, Integral i) => i -> V3 f -> V3 f -> f -> f -> g -> ([Object f], g)
randomSpheres i (V3 minX minY minZ) (V3 maxX maxY maxZ) minR maxR rGenerator 
    = if i <= 0
      then ([], rGenerator)
      else let (x, g0) = randomR (minX, maxX) rGenerator
               (y, g1) = randomR (minY, maxY) g0
               (z, g2) = randomR (minZ, maxZ) g1
               (radius, g3) = randomR (minR, maxR) g2
               (kDR, g4) = randomR (0, 1) g3
               (kDG, g5) = randomR (0, 1) g4
               (kDB, g6) = randomR (0, 1) g5
               (kSR, g7) = randomR (0, 1) g6
               (kSG, g8) = randomR (0, 1) g7
               (kSB, g9) = randomR (0, 1) g8
               (kExp, g10) = randomR (1, 5) g9
               sphere = Object (Sphere (P (V3 x y z)) radius) (PlasticMaterial (RGB kDR kDG kDB) 1 (RGB kSR kSG kSB) 1 kExp) lambertShader
               (spheres, lastG) = randomSpheres (i - 1) (V3 minX minY minZ) (V3 maxX maxY maxZ) minR maxR g10
           in (sphere : spheres, lastG)

randomSpheresPlane :: (Floating f) => Object f
randomSpheresPlane = Object (Plane (P (V3 0 (-150) 0)) (V3 0 1 0)) (MatteMaterial (RGB 1 1 1) 1) lambertShader

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
    let (intersection, material, shader) = listTrace (ListScene [Object testSphere (ColorMaterial (testRedRGB :: Color Float)) colorShader]) (testPinkRGB :: Color Float) (testHitRay :: Ray Float)
    putStrLn $ show $ (intersection, material)

testNaiveTracePlane :: IO ()
testNaiveTracePlane = do
    putStrLn "-- Testing Naive Trace with a Sphere"
    let (intersection, material, shader) = listTrace (ListScene [Object testPlane (ColorMaterial (testRedRGB :: Color Float)) colorShader]) (testPinkRGB :: Color Float) (testHitRay :: Ray Float)
    putStrLn $ show $ (intersection, material)

testRenderBasicSphere :: IO ()
testRenderBasicSphere =
    do putStrLn "-- Writing basic sphere image to basic_sphere.png"
       writePNG "basic_sphere.png"
                (pixelTraceGenerator
                 listTrace
                 traceAllLights
                 (ListScene [Object testSphere (ColorMaterial (testRedRGB :: Color Float)) colorShader])
                 [EnvironmentLight testBlackRGB]
                 (testPinkRGB :: Color Float)
                 testCamera
                 (1024, 768, 1.0 :: Float, 1.0)
                 (singleSampling orthoLens))

testRenderBasicScene :: IO ()
testRenderBasicScene =
    do putStrLn "-- Writing Suffern scene image to suffern_scene.png"
       writePNG "suffern_scene.png"
                (pixelTraceGenerator
                 listTrace
                 traceAllLights
                 (ListScene [suffernSphere0, suffernSphere1, suffernPlane])
                 [EnvironmentLight testBlackRGB]
                 (testPinkRGB :: Color Float)
                 suffernCamera
                 (200, 200, 1.0 :: Float, 1.0)
                 (singleSampling orthoLens))

testRender4xSuperSamplingBasicScene :: IO ()
testRender4xSuperSamplingBasicScene =
    do putStrLn "-- Writing Suffern scene image with 4x supersampling to suffern_scene_4x.png"
       writePNG "suffern_scene_4x.png"
                (pixelTraceGenerator
                 listTrace
                 traceAllLights
                 (ListScene [suffernSphere0, suffernSphere1, suffernPlane])
                 [EnvironmentLight testBlackRGB]
                 (testPinkRGB :: Color Float)
                 suffernCamera
                 (200, 200, 1.0 :: Float, 1.0)
                 (grid4xSampling orthoLens))

testRenderLitScene :: IO ()
testRenderLitScene =
    do putStrLn "-- Writing lit Suffern scene image to lit_suffern_scene.png"
       writePNG "lit_suffern_scene.png"
                (pixelTraceGenerator
                 listTrace
                 traceAllLights
                 (ListScene [litSuffernSphere0, litSuffernSphere1, litSuffernPlane, litSuffernAABB, litSuffernTriangle, litSuffernDisk, litSuffernRectangle])
                 [suffernLight0, suffernLight1, suffernLight2, suffernLight3]
                 (testPinkRGB :: Color Double)
                 litSuffernCamera
                 (200, 200, 1.0 :: Double, 1.0)
                 (grid4xSampling orthoLens))

testRenderRandomSpheresScene :: IO ()
testRenderRandomSpheresScene =
    do putStrLn "-- Writing random spheres scene image to random_spheres_scene.png"
       let (spheres, g) = randomSpheres 100 (V3 (-300) (-100) (-100)) (V3 300 300 100) 1 30 (mkStdGen 588025)
       writePNG "random_spheres_scene.png"
                (pixelTraceGenerator
                 listTrace
                 traceAllLights
                 (ListScene (randomSpheresPlane : spheres))
                 [suffernLight0, suffernLight1, suffernLight2, suffernLight3]
                 (testSkyBlueRGB :: Color Double)
                 randomSpheresCamera
                 (640, 480, 1.0 :: Double, 1.0)
                 (simpleSampling (perspectiveLens (pi / 3))))

main :: IO ()
main = do putStrLn "Running tests"
          putStrLn "--Suffern camera"
          putStrLn $ show $ (suffernCamera :: Camera Float)
          let ct@(CameraTransforms {w2v = worldToView, nM = normalMatrix}) = (computeCameraTransforms (suffernCamera :: Camera Float))
              s0@(Object (Sphere (P s0Position) s0Radius) s0Color colorShader) = suffernSphere0
          putStrLn "--Suffern transforms"
          putStrLn $ show $ ct
          putStrLn "--Suffern sphere"
          putStrLn $ show $ s0
          putStrLn "--Suffern sphere view space center"
          putStrLn $ show $ (worldToView !* (point s0Position))
          putStrLn "--Test Empty KD tree"
          putStrLn $ show $ (testEmptyKDTree :: KDTree Double)
          putStrLn "--Test KD tree"
          putStrLn $ show $ (testKDTree :: KDTree Double)
          testRayIntersectSphere
          testRayIntersectPlane
          testRayMissSphere
          testRayMissPlane
          testNaiveTraceSphere
          testNaiveTracePlane
          testRenderBasicSphere
          testRenderBasicScene
          testRender4xSuperSamplingBasicScene
          testRenderLitScene
          testRenderRandomSpheresScene 

