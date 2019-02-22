import Linear
import Linear.Affine

import Screen
import Color
import Geometry
import Object
import Camera

testBasicSphere :: IO ()
testBasicSphere =
    do putStrLn "Writing basic sphere image to basic_sphere.png"
       writePNG "basic_sphere.png"
                (basicNaiveTraceGenerator
                 (ViewPlane {width = 1024, height = 768, pixelSize = 1.0 :: Float, gamma = 1.0, invGamma = 1.0})
                 [(ColorObject (Sphere (P (V3 0.0 0.0 100.0)) 10.0) (RGB (V3 1.0 0.0 0.0)))])
                1024
                768

main :: IO ()
main = do putStrLn "Running tests"
          testBasicSphere
