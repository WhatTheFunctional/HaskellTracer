module Trace
    ( traceRays
    , listTrace
    ) where

import Numeric.Limits
import Linear
import Linear.Matrix
import Linear.Affine

import Color
import Ray
import Object
import Scene
import Light

initialIntersection :: (Epsilon f, Floating f, Ord f, RealFloat f) => Intersection f
initialIntersection = Intersection {intersectionPoint = (P (V3 0.0 0.0 0.0)), intersectionNormal = (V3 0.0 0.0 1.0), tMin = maxValue}

traceRays :: (Color c, Epsilon f, Floating f, Ord f, RealFloat f) => (Ray f -> c) -> [Ray f] -> c -> c
traceRays _ [] currentColor = currentColor
traceRays traceFunction (ray : rays) currentColor =
    let traceColor = (traceFunction ray)
    in traceRays traceFunction rays (mixColors traceColor currentColor)

listTraceIter :: (Color c, Epsilon f, Floating f, Ord f, RealFloat f) => ListScene f c -> (Intersection f, c) -> Ray f -> (Intersection f, c)
listTraceIter (ListScene []) (traceIntersection, traceColor) _ = (traceIntersection, traceColor)
listTraceIter (ListScene ((ColorObject shape objectColor) : objects)) (traceIntersection@(Intersection {tMin = traceTMin}), traceColor) ray =
    case rayIntersection ray shape of
        Nothing -> listTraceIter (ListScene objects) (traceIntersection, traceColor) ray
        Just objectIntersection@(Intersection {tMin = tm}) ->
            if tm < traceTMin
            then listTraceIter (ListScene objects) (objectIntersection, objectColor) ray
            else listTraceIter (ListScene objects) (traceIntersection, traceColor) ray

-- List tracer iterates through a list of objects
-- List tracer only detects hits and returns a color, it doesn't perform lighting
listTrace :: (Color c, Epsilon f, Floating f, Ord f, RealFloat f) => ListScene f c -> [Light f] -> M44 f -> M44 f -> c -> Ray f -> c
listTrace (ListScene objects) lights worldToView normalMatrix bgColor ray = 
    let transformedObjects = fmap (transformObject worldToView normalMatrix) objects
        transformedLights = fmap (transformLight worldToView normalMatrix) lights
        (traceIntersection, traceColor) = listTraceIter (ListScene transformedObjects) (initialIntersection, bgColor) ray
    in traceColor

