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

listTrace :: (Epsilon f, RealFloat f, Ord f) => Scene f -> Color f -> Ray f -> (Intersection f, Material f, (ShadePoint f -> Color f))
listTrace (ListScene objects) bgColor ray = 
    foldr (\(Object shape objectMaterial objectShader) (intersection@(Intersection {tMin = traceTMin}), material, shader) ->
               case rayIntersection ray shape of
                   Nothing -> (intersection, material, shader)
                   Just objectIntersection@(Intersection {tMin = tm}) ->
                       if tm < traceTMin
                       then (objectIntersection, objectMaterial, objectShader)
                       else (intersection, material, shader)) (initialIntersection, (ColorMaterial bgColor), colorShader) objects

-- Light tracer

traceAllLights :: (Epsilon f, RealFloat f, Ord f) => (Ray f -> (Intersection f, Material f, (ShadePoint f -> Color f))) -> [Light f] -> Color f -> Ray f -> (Intersection f, Material f, (ShadePoint f -> Color f)) -> Color f
traceAllLights traceFunction lights bgColor ray (intersection, ColorMaterial color, shader) = color
traceAllLights traceFunction lights bgColor ray (intersection@(Intersection {intersectionPoint = point, intersectionNormal = normal, tMin = traceTMin}), material, shader) =
    let innerGetLightRay = getLightRay point
    in if traceTMin == maxValue
       then bgColor
       else foldr (\light accumulatedColor ->
                       let (lightRay, lightT) = innerGetLightRay light
                           (Intersection {tMin = lightTMin}, _, _) = traceFunction lightRay
                       in if lightT <= lightTMin
                          then (shadeLight material point normal shader ray lightRay (getLightColor light)) ^+^ accumulatedColor
                          else accumulatedColor) (pure 0) lights

