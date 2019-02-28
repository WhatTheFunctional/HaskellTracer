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

initialIntersection :: (RealFloat f) => Intersection f
initialIntersection = Intersection {intersectionPoint = (P (V3 0 0 0)), intersectionNormal = (V3 0 0 1), tMin = maxValue}

traceRays :: (Num f) => (Ray f -> Color f) -> [Ray f] -> Color f -> Color f
traceRays _ [] currentColor = currentColor
traceRays traceFunction (ray : rays) currentColor =
    let traceColor = (traceFunction ray)
    in traceRays traceFunction rays (traceColor ^+^ currentColor)

listTraceIter :: (Epsilon f, Floating f, Ord f) => ListScene f -> (Intersection f, Material f, (V3 f -> Material f -> V3 f -> V3 f -> Color f)) -> Ray f -> (Intersection f, Material f, (V3 f -> Material f -> V3 f -> V3 f -> Color f))
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
listTrace :: (Epsilon f, RealFloat f, Ord f) => ListScene f -> [Light f] -> M44 f -> M44 f -> Color f -> Ray f -> Color f
listTrace (ListScene objects) lights worldToView normalMatrix bgColor ray = 
    let transformedObjects = fmap (transformObject worldToView normalMatrix) objects
        transformedLights = fmap (transformLight worldToView normalMatrix) lights
        (traceIntersection, traceMaterial, traceShader) = listTraceIter (ListScene transformedObjects) (initialIntersection, (ColorMaterial bgColor), colorShader) ray
        traceColor = shadeAllLights traceIntersection traceMaterial traceShader ray lights
    in traceColor

