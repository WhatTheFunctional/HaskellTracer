module Trace
    ( traceRays
    , listTrace
    ) where

import Numeric.Limits
import Linear.Epsilon

import Color
import Ray
import Object
import Scene

traceRays :: (Epsilon f, Ord f, Floating f, RealFloat f, Color c) => (Ray f -> c) -> [Ray f] -> c -> c
traceRays _ [] currentColor = currentColor
traceRays traceFunction (ray : rays) currentColor =
    let traceColor = (traceFunction ray)
    in traceRays traceFunction rays (mixColors traceColor currentColor)

listTraceIter :: (Epsilon f, Ord f, Floating f, Color c) => c -> ListScene f c -> f -> Ray f -> c
listTraceIter traceColor (ListScene []) _ _ = traceColor
listTraceIter traceColor (ListScene ((ColorObject shape objectColor) : objects)) traceTMin ray =
    case rayIntersection ray shape of
        Nothing -> listTraceIter traceColor (ListScene objects) traceTMin ray
        Just (Intersection {tMin = tm}) -> if tm < traceTMin
                                           then listTraceIter objectColor (ListScene objects) tm ray
                                           else listTraceIter traceColor (ListScene objects) traceTMin ray

-- List tracer iterates through a list of objects
-- List tracer only detects hits and returns a color, it doesn't perform lighting
listTrace :: (Epsilon f, Ord f, Floating f, RealFloat f, Color c) => c -> ListScene f c -> Ray f -> c
listTrace bgColor listScene = listTraceIter bgColor listScene maxValue

