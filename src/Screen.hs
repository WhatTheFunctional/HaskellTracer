module Screen
    ( basicNaiveTraceGenerator
    , writePNG
    ) where

import Codec.Picture
import Linear
import Linear.Affine

import Geometry
import Color
import Camera
import Object
import Trace

basicNaiveTraceGenerator :: (Epsilon f, Ord f, Floating f, Integral i, Color c) => ViewPlane f i -> [Object f c] -> (Int -> Int -> PixelRGB8)
basicNaiveTraceGenerator (ViewPlane {width = w, height = h, pixelSize = s, gamma = g, invGamma = ig}) objects =
    (\x y -> let tx = ((fromIntegral (x - (fromIntegral w))) / 2.0 + 0.5) * s
                 ty = ((fromIntegral (y - (fromIntegral h))) / 2.0 + 0.5) * s
                 ray = Ray {rayOrigin = P (V3 tx ty 0.0), rayDirection = (V3 0.0 0.0 1.0)}
                 color = naiveTrace ray objects
             in toPixelRGB8 color)

writePNG :: String -> (Int -> Int -> PixelRGB8) -> Int -> Int -> IO ()
writePNG fileName generator width height = savePngImage fileName (ImageRGB8 (generateImage generator width height))

