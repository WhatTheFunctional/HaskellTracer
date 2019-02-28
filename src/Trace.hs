module Trace
    ( listTrace
    , traceAllLights
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

-- List tracer iterates through a list of objects
-- List tracer only detects hits and returns a tuple containing the intersection point, material, and shader
listTrace :: (Epsilon f, RealFloat f, Ord f) => Scene f -> M44 f -> M44 f -> Color f -> Ray f -> (Intersection f, Material f, (V3 f -> Material f -> V3 f -> V3 f -> Color f))
listTrace (ListScene objects) worldToView normalMatrix bgColor ray = 
    foldr (\(Object shape objectMaterial objectShader) (intersection@(Intersection {tMin = traceTMin}), material, shader) ->
               case rayIntersection ray shape of
                   Nothing -> (intersection, material, shader)
                   Just objectIntersection@(Intersection {tMin = tm}) ->
                       if tm < traceTMin
                       then (objectIntersection, objectMaterial, objectShader)
                       else (intersection, material, shader)) (initialIntersection, (ColorMaterial bgColor), colorShader) objects

traceAllLights :: (Epsilon f, RealFloat f, Ord f) => s -> [Light f] -> M44 f -> M44 f -> Color f -> Ray f -> (Intersection f, Material f, (V3 f -> Material f -> V3 f -> V3 f -> Color f)) -> Color f
traceAllLights scene lights worldToView normalMatrix bgColor ray (intersection@(Intersection {intersectionPoint = point, intersectionNormal = normal, tMin = traceTMin}), material, shader) =
    let innerGetLightRay = getLightRay point
    in if traceTMin == maxValue
       then bgColor
       else foldr (\light accumulatedColor ->
                       (shadeLight point normal material shader ray (innerGetLightRay light) (getLightColor light)) ^+^ accumulatedColor) (pure 0) lights

