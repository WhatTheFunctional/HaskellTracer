module Trace
    ( traceRays
    , listTrace
    ) where

import Numeric.Limits
import Linear.Epsilon

import Color
import Ray
import Object

traceRays :: (Epsilon f, Ord f, Floating f, RealFloat f, Color c) => (Ray f -> c) -> [Ray f] -> c -> c
traceRays _ [] currentColor = currentColor
traceRays traceFunction (ray : rays) currentColor =
    let traceColor = (traceFunction ray)
    in traceRays traceFunction rays (mixColors traceColor currentColor)

listTraceIter :: (Epsilon f, Ord f, Floating f, Color c) => c -> [Object f c] -> Ray f -> f -> c
listTraceIter traceColor [] ray traceTMin = traceColor
listTraceIter traceColor ((ColorObject shape objectColor) : objects) ray traceTMin =
    case rayIntersection ray shape of
        Nothing -> listTraceIter traceColor objects ray traceTMin 
        Just (Intersection {tMin = tm}) -> if tm < traceTMin
                                           then listTraceIter objectColor objects ray tm 
                                           else listTraceIter traceColor objects ray traceTMin

-- List tracer iterates through a list of objects
-- List tracer only detects hits and returns a color, it doesn't perform lighting
listTrace :: (Epsilon f, Ord f, Floating f, RealFloat f, Color c) => c -> [Object f c] -> Ray f -> c
listTrace bgColor objects ray = listTraceIter bgColor objects ray maxValue

