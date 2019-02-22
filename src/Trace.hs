module Trace
    ( naiveTrace
    ) where

import Numeric.Limits
import Linear
import Color
import Geometry
import Object

naiveTraceIter :: (Epsilon f, Ord f, Floating f, Color c) => Ray f -> [Object f c] -> f -> c -> c
naiveTraceIter ray [] traceTMin traceColor = traceColor
naiveTraceIter ray ((ColorObject shape objectColor) : objects) traceTMin traceColor =
    case rayIntersection ray shape of
        Nothing -> naiveTraceIter ray objects traceTMin traceColor
        Just (Intersection {tMin = tm}) -> if tm < traceTMin
                                           then naiveTraceIter ray objects tm objectColor
                                           else naiveTraceIter ray objects traceTMin traceColor

-- Naive tracer iterates through a list of objects
-- Naive tracer only detects hits and returns a color, it doesn't perform lighting
naiveTrace :: (Epsilon f, Ord f, Floating f, RealFloat f, Color c) => c -> Ray f -> [Object f c] -> c
naiveTrace bgColor ray objects = naiveTraceIter ray objects maxValue bgColor

