module Screen
    ( listTraceGenerator
    ) where

import Codec.Picture
import Linear
import Linear.Affine

import Geometry
import Color
import Camera
import Object
import Trace

listTraceGenerator :: (Epsilon f, Ord f, Floating f, RealFloat f, Integral i, Color c) => c -> ViewPlane f i -> [Object f c] -> (f -> f -> [Ray f]) -> (Int -> Int -> PixelRGB8)
listTraceGenerator bgColor (ViewPlane {width = w, height = h, pixelSize = s, gamma = g, invGamma = ig}) objects lensFunction =
    (\x y -> let tx = (((fromIntegral w) / 2.0) - (fromIntegral x) + 0.5) * s
                 ty = ((fromIntegral y) - ((fromIntegral h) / 2.0) + 0.5) * s
                 rays = lensFunction tx ty
                 color = traceRays (listTrace bgColor objects) rays blackColor
             in toPixelRGB8 color)

