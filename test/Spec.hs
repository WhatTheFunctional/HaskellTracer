import Linear
import Linear.Affine

import Color
import Geometry
import Object
import Trace
import Lens
import Camera
import Screen
import IO

testSphere :: (Epsilon f, Ord f, Floating f) => Shape f
testSphere = Sphere (P (V3 0.0 0.0 100.0)) 100.0

testHitRay :: (Epsilon f, Ord f, Floating f) => Ray f
testHitRay = Ray (P (V3 0.0 0.0 0.0)) (V3 0.0 0.0 1.0)

testMissRay :: (Epsilon f, Ord f, Floating f) => Ray f
testMissRay = Ray (P (V3 0.0 0.0 0.0)) (V3 0.0 0.0 1.0)

testRedRGB :: (RealFrac f, Epsilon f, Ord f, Floating f) => RGB f
testRedRGB = makeRGB 1.0 0.0 0.0

testGreenRGB :: (RealFrac f, Epsilon f, Ord f, Floating f) => RGB f
testGreenRGB = makeRGB 0.0 1.0 0.0

testPinkRGB :: (RealFrac f, Epsilon f, Ord f, Floating f) => RGB f
testPinkRGB = makeRGB 1.0 0.0 1.0

testCamera :: (RealFrac f, Epsilon f, Ord f, Floating f) => Camera f
testCamera = Camera (P (V3 0.0 0.0 0.0)) (V3 0.0 0.0 1.0) (V3 0.0 1.0 0.0)

testRayIntersectSphere :: IO ()
testRayIntersectSphere = do
    putStrLn "-- Testing Ray Sphere Intersection"
    putStrLn $ show $ rayIntersection (testHitRay :: Ray Float) testSphere

testRayMissSphere :: IO ()
testRayMissSphere = do
    putStrLn "-- Testing Ray Sphere Miss"
    putStrLn $ show $ rayIntersection (testMissRay :: Ray Float) testSphere

testNaiveTraceSphere :: IO ()
testNaiveTraceSphere = do
    putStrLn "-- Testing Naive Trace with a Sphere"
    putStrLn $ show $ listTrace (testPinkRGB :: RGB Float) [ColorObject testSphere (testRedRGB :: RGB Float)] (testHitRay :: Ray Float)

testRenderBasicSphere :: IO ()
testRenderBasicSphere =
    do putStrLn "-- Writing basic sphere image to basic_sphere.png"
       writePNG "basic_sphere.png"
                (listTraceGenerator
                 (testPinkRGB :: RGB Float)
                 testCamera
                 (ViewPlane {width = 1024, height = 768, pixelSize = 1.0 :: Float, gamma = 1.0, invGamma = 1.0})
                 [ColorObject testSphere (testRedRGB :: RGB Float)]
                 orthoLensSingle)
                1024
                768

main :: IO ()
main = do putStrLn "Running tests"
          testRayIntersectSphere
          testRayMissSphere
          testNaiveTraceSphere
          testRenderBasicSphere
