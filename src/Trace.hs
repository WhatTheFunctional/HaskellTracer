module Trace
    ( naiveTrace
    ) where

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

naiveTrace :: (Epsilon f, Ord f, Floating f, Color c) => Ray f -> [Object f c] -> c
naiveTrace ray objects = naiveTraceIter ray objects 0.0 backgroundColor

