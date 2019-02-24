module Trace
    ( traceRays
    , listTrace
    ) where

import Numeric.Limits
import Linear.Matrix
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

listTraceIter :: (Epsilon f, Ord f, Floating f, Color c) => ListScene f c -> c -> f -> Ray f -> c
listTraceIter (ListScene []) traceColor _ _ = traceColor
listTraceIter (ListScene ((ColorObject shape objectColor) : objects)) traceColor traceTMin ray =
    case rayIntersection ray shape of
        Nothing -> listTraceIter (ListScene objects) traceColor traceTMin ray
        Just (Intersection {tMin = tm}) -> if tm < traceTMin
                                           then listTraceIter (ListScene objects) objectColor tm ray
                                           else listTraceIter (ListScene objects) traceColor traceTMin ray

-- List tracer iterates through a list of objects
-- List tracer only detects hits and returns a color, it doesn't perform lighting
listTrace :: (Epsilon f, Ord f, Floating f, RealFloat f, Color c) => ListScene f c -> M44 f -> M44 f -> c -> Ray f -> c
listTrace (ListScene objects) viewToWorld worldToView bgColor = 
    let transformedObjects = fmap (transformObject viewToWorld worldToView) objects
    in listTraceIter (ListScene transformedObjects) bgColor maxValue

