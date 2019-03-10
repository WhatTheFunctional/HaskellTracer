module Trace
    ( TraceResult (..)
    , traceRays
    , traceAllLights
    , traceOneLight
    ) where

import Control.Parallel
import Numeric.Limits
import Data.List
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
import Sampling

data TraceResult = TraceResult Intersection Material (ShadePoint -> Color Double)

instance Show TraceResult where
    show (TraceResult intersection material shaderFunction) = "TraceResult (" ++ (show intersection) ++ ") (" ++ (show material) ++ ")"

initialIntersection :: Intersection
initialIntersection = Intersection {intersectionPoint = (P (V3 0 0 0)), intersectionNormal = (V3 0 0 1), tMin = infinity}

emptyTraceResult :: Color Double -> TraceResult
emptyTraceResult bgColor = TraceResult initialIntersection (ColorMaterial bgColor) colorShader

-- List tracer iterates through a list of objects

traceRays :: (LowDiscrepancySequence s)
          => Scene
          -> Color Double
          -> Ray
          -> s
          -> ((TraceResult, Ray), s)

traceRays (ListScene objects) bgColor ray gen = 
    ((foldl' (\traceResult@(TraceResult (Intersection {tMin = traceTMin}) material shader) (Object shape objectMaterial objectShader) ->
                  case rayIntersection ray shape of
                      Nothing -> traceResult
                      Just objectIntersection@(Intersection {tMin = tm}) ->
                          if tm < traceTMin
                          then TraceResult objectIntersection objectMaterial objectShader
                          else traceResult) (emptyTraceResult bgColor) objects, ray), gen)

traceRays (KDScene (KDTree aabb planes node)) bgColor ray gen =
    let ((planeTraceResult@(TraceResult (Intersection {tMin = planeTMin}) _ _), planeRay), gen1) = traceRays (ListScene planes) bgColor ray gen
    in case rayIntersection ray aabb of
           Nothing -> ((planeTraceResult, planeRay), gen1)
           Just objectIntersection ->
               let ((kdTraceResult@(TraceResult (Intersection {tMin = kdTMin}) _ _), kdRay), gen2) = traceKDNode node aabb bgColor ray gen1
               in if kdTMin < planeTMin
                  then ((kdTraceResult, kdRay), gen2)
                  else ((planeTraceResult, planeRay), gen2)

-- KD node tracer

traceKDNode :: (LowDiscrepancySequence s)
            => KDNode
            -> Shape
            -> Color Double
            -> Ray
            -> s
            -> ((TraceResult, Ray), s)

traceKDNode (KDLeaf objects) _ bgColor ray gen = traceRays (ListScene objects) bgColor ray gen

traceKDNode (KDBranch split left right) aabb bgColor ray gen =
    let (leftAABB, rightAABB) = splitBoundingBox split aabb
        tLeft = case rayIntersection ray leftAABB of
                    Nothing -> infinity
                    Just objectIntersection@(Intersection {tMin = leftTMin}) -> leftTMin
        tRight = case rayIntersection ray rightAABB of
                    Nothing -> infinity
                    Just objectIntersection@(Intersection {tMin = rightTMin}) -> rightTMin
        leftIntersection = tLeft /= infinity
        rightIntersection = tRight /= infinity
    in if leftIntersection && rightIntersection
       then if tLeft <= tRight
            then let ((leftTraceResult@(TraceResult (Intersection {tMin = leftTMin}) _ _), leftRay), gen1) = traceKDNode left leftAABB bgColor ray gen
                 in if leftTMin /= infinity
                    then ((leftTraceResult, leftRay), gen1)
                    else let ((rightTraceResult@(TraceResult (Intersection {tMin = rightTMin}) _ _), rightRay), gen2) = traceKDNode right rightAABB bgColor ray gen1
                         in if rightTMin /= infinity
                            then ((rightTraceResult, rightRay), gen2)
                            else ((emptyTraceResult bgColor, ray), gen2)
            else let ((rightTraceResult@(TraceResult (Intersection {tMin = rightTMin}) _ _), rightRay), gen1) = traceKDNode right rightAABB bgColor ray gen
                 in if rightTMin /= infinity
                    then ((rightTraceResult, rightRay), gen1)
                    else let ((leftTraceResult@(TraceResult (Intersection {tMin = leftTMin}) _ _), leftRay), gen2) = traceKDNode left leftAABB bgColor ray gen1
                         in if leftTMin /= infinity
                            then ((leftTraceResult, leftRay), gen2)
                            else ((emptyTraceResult bgColor, ray), gen2)
        else if leftIntersection
             then let ((leftTraceResult@(TraceResult (Intersection {tMin = leftTMin}) _ _), leftRay), gen1) = traceKDNode left leftAABB bgColor ray gen
                  in if leftTMin /= infinity
                     then ((leftTraceResult, leftRay), gen1)
                     else ((emptyTraceResult bgColor, ray), gen1)
             else if rightIntersection
                  then let ((rightTraceResult@(TraceResult (Intersection {tMin = rightTMin}) _ _), rightRay), gen1) = traceKDNode right rightAABB bgColor ray gen
                       in if rightTMin /= infinity
                          then ((rightTraceResult, rightRay), gen1)
                          else ((emptyTraceResult bgColor, ray), gen1)
                  else ((emptyTraceResult bgColor, ray), gen)

-- All lights tracer

traceAllLights :: (LowDiscrepancySequence s)
               => (Ray
                   -> s
                   -> ((TraceResult, Ray), s))
               -> [Light]
               -> Color Double
               -> (TraceResult, Ray)
               -> s
               -> (Color Double, s)
traceAllLights traceFunction lights bgColor ((TraceResult _ (ColorMaterial color) _), ray) gen0 = (color, gen0)
traceAllLights traceFunction lights bgColor ((TraceResult (Intersection {intersectionPoint = point, intersectionNormal = normal, tMin = traceTMin}) material shader), ray) gen0 =
    let innerGetLightRay = getLightRay point
    in if traceTMin == infinity
       then (bgColor, gen0)
       else let numLights = length lights
                (color, gen3) = foldl' (\(accumulatedColor, gen1) light ->
                                            let ((lightRay, lightT), gen2) = innerGetLightRay light gen1
                                                (((TraceResult (Intersection {tMin = lightTMin}) _ _), traceRay), gen3) = traceFunction lightRay gen2
                                            in if lightT <= lightTMin
                                               then ((shadeLight material point normal shader ray lightRay (getLightColor light)) ^+^ accumulatedColor, gen3)
                                               else (accumulatedColor, gen3)) ((pure 0), gen0) lights
            in (color ^/ (fromIntegral numLights), gen3)

-- One light tracer (Random)

traceOneLight :: (LowDiscrepancySequence s)
              => (Ray
                  -> s
                  -> ((TraceResult, Ray), s))
              -> [Light]
              -> Color Double
              -> (TraceResult, Ray)
              -> s
              -> (Color Double, s)
traceOneLight traceFunction lights bgColor ((TraceResult _ (ColorMaterial color) _), ray) gen0 = (color, gen0)
traceOneLight traceFunction lights bgColor ((TraceResult (Intersection {intersectionPoint = point, intersectionNormal = normal, tMin = traceTMin}) material shader), ray) gen0 =
    if traceTMin == infinity
       then (bgColor, gen0)
       else let numLights = fromIntegral (length lights)
                (lightIndex, gen1) = sampleR (0, numLights) gen0
                light = lights !! (floor lightIndex)
                ((lightRay, lightT), gen2) = getLightRay point light gen1
                (((TraceResult (Intersection {tMin = lightTMin}) _ _), traceRay), gen3) = traceFunction lightRay gen2
            in if lightT <= lightTMin
               then (shadeLight material point normal shader ray lightRay (getLightColor light), gen3)
               else (pure 0, gen3)

