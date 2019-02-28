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
import Material
import Shading

initialIntersection :: (Epsilon f, Floating f, Ord f, RealFloat f) => Intersection f
initialIntersection = Intersection {intersectionPoint = (P (V3 0.0 0.0 0.0)), intersectionNormal = (V3 0.0 0.0 1.0), tMin = maxValue}

traceRays :: (Color c, Epsilon f, Floating f, Ord f, RealFloat f) => (Ray f -> c) -> [Ray f] -> c -> c
traceRays _ [] currentColor = currentColor
traceRays traceFunction (ray : rays) currentColor =
    let traceColor = (traceFunction ray)
    in traceRays traceFunction rays (mixColors traceColor currentColor)

listTraceIter :: (Color c, Epsilon f, Floating f, Ord f, RealFloat f) => ListScene f c -> (Intersection f, Material f c, (Intersection f -> Material f c -> c)) -> Ray f -> (Intersection f, Material f c, (Intersection f -> Material f c -> c))
listTraceIter (ListScene []) (traceIntersection, traceMaterial, traceShader) _ = (traceIntersection, traceMaterial, traceShader)
listTraceIter (ListScene ((Object shape objectMaterial objectShader) : objects)) (traceIntersection@(Intersection {tMin = traceTMin}), traceMaterial, traceShader) ray =
    case rayIntersection ray shape of
        Nothing -> listTraceIter (ListScene objects) (traceIntersection, traceMaterial, traceShader) ray
        Just objectIntersection@(Intersection {tMin = tm}) ->
            if tm < traceTMin
            then listTraceIter (ListScene objects) (objectIntersection, objectMaterial, objectShader) ray
            else listTraceIter (ListScene objects) (traceIntersection, traceMaterial, traceShader) ray

-- List tracer iterates through a list of objects
-- List tracer only detects hits and returns a color, it doesn't perform lighting
listTrace :: (Color c, Epsilon f, Floating f, Ord f, RealFloat f) => ListScene f c -> [Light f] -> M44 f -> M44 f -> c -> Ray f -> c
listTrace (ListScene objects) lights worldToView normalMatrix bgColor ray = 
    let transformedObjects = fmap (transformObject worldToView normalMatrix) objects
        transformedLights = fmap (transformLight worldToView normalMatrix) lights
        (traceIntersection, traceMaterial, traceShader) = listTraceIter (ListScene transformedObjects) (initialIntersection, (ColorMaterial bgColor), colorShader) ray
        traceColor = traceShader traceIntersection traceMaterial
    in traceColor

