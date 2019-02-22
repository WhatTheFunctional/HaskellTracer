import Linear
import Linear.Affine

import Screen
import Color
import Geometry
import Object
import Camera
import Trace

testSphere :: (Epsilon f, Ord f, Floating f) => Shape f
testSphere = Sphere (P (V3 0.0 0.0 100.0)) 100.0

testHitRay :: (Epsilon f, Ord f, Floating f) => Ray f
testHitRay = Ray (P (V3 0.0 0.0 0.0)) (V3 0.0 0.0 1.0)

testMissRay :: (Epsilon f, Ord f, Floating f) => Ray f
testMissRay = Ray (P (V3 0.0 0.0 0.0)) (V3 0.0 0.0 1.0)

testRedRGB :: (Epsilon f, Ord f, Floating f) => RGB f
testRedRGB = (RGB (V3 1.0 0.0 0.0))

testGreenRGB :: (Epsilon f, Ord f, Floating f) => RGB f
testGreenRGB = (RGB (V3 0.0 1.0 0.0))

testPinkRGB :: (Epsilon f, Ord f, Floating f) => RGB f
testPinkRGB = (RGB (V3 1.0 0.0 1.0))

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
    putStrLn $ show $ naiveTrace (testPinkRGB :: RGB Float) (testHitRay :: Ray Float) [ColorObject testSphere (testRedRGB :: RGB Float)]

testRenderBasicSphere :: IO ()
testRenderBasicSphere =
    do putStrLn "-- Writing basic sphere image to basic_sphere.png"
       writePNG "basic_sphere.png"
                (orthoNaiveTraceGenerator
                 (testPinkRGB :: RGB Float)
                 (ViewPlane {width = 1024, height = 768, pixelSize = 1.0 :: Float, gamma = 1.0, invGamma = 1.0})
                 [ColorObject testSphere (testRedRGB :: RGB Float)])
                1024
                768

main :: IO ()
main = do putStrLn "Running tests"
          testRayIntersectSphere
          testRayMissSphere
          testNaiveTraceSphere
          testRenderBasicSphere
