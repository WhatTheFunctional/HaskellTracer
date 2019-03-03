module Trace
    ( traceRays
    , traceAllLights
    ) where

import Numeric.Limits
import Linear
import Linear.Affine

import Color
import Ray
import Geometry
import Object
import Scene
import Light
import Material
import Shading
import Accelerator


initialIntersection :: (RealFloat f) => Intersection f
initialIntersection = Intersection {intersectionPoint = (P (V3 0 0 0)), intersectionNormal = (V3 0 0 1), tMin = infinity}

emptyIntersectionTuple :: (RealFloat f) => Color f -> (Intersection f, Material f, (ShadePoint f -> Color f))
emptyIntersectionTuple bgColor = (initialIntersection, (ColorMaterial bgColor), colorShader)

-- List tracer iterates through a list of objects

traceRays :: (Epsilon f, RealFloat f, Ord f) => Scene f -> Color f -> Ray f -> (Intersection f, Material f, (ShadePoint f -> Color f))

traceRays (ListScene objects) bgColor ray = 
    foldr (\(Object shape objectMaterial objectShader) (intersection@(Intersection {tMin = traceTMin}), material, shader) ->
               case rayIntersection ray shape of
                   Nothing -> (intersection, material, shader)
                   Just objectIntersection@(Intersection {tMin = tm}) ->
                       if tm < traceTMin
                       then (objectIntersection, objectMaterial, objectShader)
                       else (intersection, material, shader)) (emptyIntersectionTuple bgColor) objects

traceRays (KDScene (KDTree aabb node)) bgColor ray = 
    case rayIntersection ray aabb of
        Nothing -> (emptyIntersectionTuple bgColor)
        Just objectIntersection -> traceKDNode node aabb bgColor ray

-- KD node tracer

traceKDNode :: (Epsilon f, RealFloat f) => KDNode f -> Shape f -> Color f -> Ray f -> (Intersection f, Material f, (ShadePoint f -> Color f))

traceKDNode (KDLeaf objects) _ bgColor ray = traceRays (ListScene objects) bgColor ray

traceKDNode (KDBranch split left right) aabb bgColor ray =
    let (leftAABB, rightAABB) = splitBoundingBox split aabb
        tLeft = case rayIntersection ray leftAABB of
                    Nothing -> infinity
                    Just objectIntersection@(Intersection {tMin = tm}) -> tm
        tRight = case rayIntersection ray rightAABB of
                    Nothing -> infinity
                    Just objectIntersection@(Intersection {tMin = tm}) -> tm
    in if tLeft < tRight
       then let leftTuple@(Intersection {tMin = leftTMin}, _, _) = traceKDNode left leftAABB bgColor ray
            in if leftTMin /= infinity
               then leftTuple
               else let rightTuple@(Intersection {tMin = rightTMin}, _, _) = traceKDNode right rightAABB bgColor ray
                    in if rightTMin /= infinity
                       then rightTuple
                       else emptyIntersectionTuple bgColor
       else let rightTuple@(Intersection {tMin = rightTMin}, _, _) = traceKDNode right rightAABB bgColor ray
            in if rightTMin /= infinity
               then rightTuple
               else let leftTuple@(Intersection {tMin = leftTMin}, _, _) = traceKDNode left leftAABB bgColor ray
                    in if leftTMin /= infinity
                       then leftTuple
                       else emptyIntersectionTuple bgColor

-- Light tracer

traceAllLights :: (Epsilon f, RealFloat f, Ord f) => (Ray f -> (Intersection f, Material f, (ShadePoint f -> Color f))) -> [Light f] -> Color f -> Ray f -> (Intersection f, Material f, (ShadePoint f -> Color f)) -> Color f
traceAllLights traceFunction lights bgColor ray (intersection, ColorMaterial color, shader) = color
traceAllLights traceFunction lights bgColor ray (intersection@(Intersection {intersectionPoint = point, intersectionNormal = normal, tMin = traceTMin}), material, shader) =
    let innerGetLightRay = getLightRay point
    in if traceTMin == infinity
       then bgColor
       else foldr (\light accumulatedColor ->
                       let (lightRay, lightT) = innerGetLightRay light
                           (Intersection {tMin = lightTMin}, _, _) = traceFunction lightRay
                       in if lightT <= lightTMin
                          then (shadeLight material point normal shader ray lightRay (getLightColor light)) ^+^ accumulatedColor
                          else accumulatedColor) (pure 0) lights

