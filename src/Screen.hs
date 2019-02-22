module Screen
    ( naiveTraceGenerator
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

naiveTraceGenerator :: (Epsilon f, Ord f, Floating f, RealFloat f, Integral i, Color c) => c -> ViewPlane f i -> [Object f c] -> (f -> f -> Ray f) -> (Int -> Int -> PixelRGB8)
naiveTraceGenerator bgColor (ViewPlane {width = w, height = h, pixelSize = s, gamma = g, invGamma = ig}) objects lensFunction =
    (\x y -> let tx = (((fromIntegral w) / 2.0) - (fromIntegral x) + 0.5) * s
                 ty = ((fromIntegral y) - ((fromIntegral h) / 2.0) + 0.5) * s
                 ray = lensFunction tx ty
                 color = naiveTrace bgColor ray objects
             in toPixelRGB8 color)

writePNG :: String -> (Int -> Int -> PixelRGB8) -> Int -> Int -> IO ()
writePNG fileName generator width height = savePngImage fileName (ImageRGB8 (generateImage generator width height))

