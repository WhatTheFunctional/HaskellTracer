module IO
    ( writePNG
    ) where

import Codec.Picture

writePNG :: String -> (Int -> Int -> PixelRGB8) -> Int -> Int -> IO ()
writePNG fileName generator width height = savePngImage fileName (ImageRGB8 (generateImage generator width height))

