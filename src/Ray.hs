module Ray
    ( Ray (..)
    , Intersection (..)
    , rayIntersection
    ) where

import Numeric.Limits
import Linear
import Linear.Affine
import Linear.Metric

import Geometry

data Ray f = Ray { rayOrigin :: Point V3 f
                 , rayDirection :: V3 f
                 } deriving (Show, Eq)

data Intersection f = Intersection { intersectionPoint :: Point V3 f
                                   , intersectionNormal :: V3 f
                                   , tMin :: f
                                   } deriving (Show, Eq)

rayEpsilon :: (Fractional f) => f
rayEpsilon = 1e-5

rayIntersection :: (Epsilon f, Floating f, Ord f) => Ray f -> Shape f -> Maybe (Intersection f)

rayIntersection (Ray {rayOrigin = ro, rayDirection = rd}) (Plane planePoint planeNormal) = 
    let denominator = (rd `dot` planeNormal)
    in if (denominator > -rayEpsilon) && (denominator < rayEpsilon)
       then Nothing
       else let t = (planePoint .-. ro) `dot` (planeNormal ^/ denominator)
            in if t <= rayEpsilon
               then Nothing
               else Just (Intersection {intersectionPoint = ro .+^ (rd ^* t), intersectionNormal = planeNormal, tMin = t})

rayIntersection (Ray {rayOrigin = ro, rayDirection = rd}) (Sphere sphereCenter sphereRadius) =
    let temp = ro .-. sphereCenter
        a = rd `dot` rd
        b = (temp `dot` rd) * 2
        c = temp `dot` temp - sphereRadius * sphereRadius
        disc = b * b - 4 * a * c
    in if disc < 0
       then Nothing
       else let e = sqrt disc
                denom = 2 * a
                t = ((-b) - e) / denom
            in if t > rayEpsilon
               then Just (Intersection {intersectionPoint = ro .+^ (rd ^* t), intersectionNormal = normalize ((temp ^+^ (rd ^* t)) ^/ sphereRadius), tMin = t})
               else let bigT = ((-b) + e) / denom
                    in if bigT > rayEpsilon
                       then Just (Intersection {intersectionPoint = ro .+^ (rd ^* bigT), intersectionNormal = normalize ((temp ^+^ (rd ^* bigT)) ^/ sphereRadius), tMin = bigT})
                       else Nothing

rayIntersection (Ray {rayOrigin = ro, rayDirection = rd}) (AABB frame v0 v1) =
    let invFrame = inv44 frame
        tro = P (normalizePoint (invFrame !* (point (unP ro))))
        (V4 tRDX tRDY tRDZ tRDW) = invFrame !* (vector rd)
        trd = V3 tRDX tRDY tRDZ
        (invRDX, invRDY, invRDZ) = (1 / tRDX, 1 / tRDY, 1 / tRDZ)
        invR = V3 invRDX invRDY invRDZ
        V3 tMinX tMinY tMinZ = (\o invRD b0 b1 ->
                                    if invRD >= 0
                                    then (b0 - o) * invRD
                                    else (b1 - o) * invRD) <$> unP tro <*> invR <*> v0 <*> v1
        V3 tMaxX tMaxY tMaxZ = (\o invRD b0 b1 ->
                                    if invRD >= 0
                                    then (b1 - o) * invRD
                                    else (b0 - o) * invRD) <$> unP tro <*> invR <*> v0 <*> v1
        minXTuple = (tMinX, if invRDX >= 0.0 then V3 (-1) 0 0 else V3 1 0 0)
        minYTuple = (tMinY, if invRDY >= 0.0 then V3 0 (-1) 0 else V3 0 1 0)
        minZTuple = (tMinZ, if invRDZ >= 0.0 then V3 0 0 (-1) else V3 0 0 1)
        (t0, inNormal) = if tMinX > tMinY
                         then if tMinZ > tMinX then minZTuple else minXTuple
                         else if tMinZ > tMinY then minZTuple else minYTuple
        maxXTuple = (tMaxX, if invRDX >= 0.0 then V3 1 0 0 else V3 (-1) 0 0)
        maxYTuple = (tMaxY, if invRDY >= 0.0 then V3 0 1 0 else V3 0 (-1) 0)
        maxZTuple = (tMaxZ, if invRDZ >= 0.0 then V3 0 0 1 else V3 0 0 (-1))
        (t1, outNormal) = if tMaxX < tMaxY
                          then if tMaxZ < tMaxX then maxZTuple else maxXTuple
                          else if tMaxZ < tMaxY then maxZTuple else maxYTuple
        (V4 inX inY inZ inW) = frame !* (vector inNormal)
        (V4 outX outY outZ outW) = frame !* (vector outNormal)
    in if (t0 < t1) && (t1 > rayEpsilon)
       then if (t0 > rayEpsilon)
            then Just (Intersection {intersectionPoint = ro .+^ (rd ^* t0), intersectionNormal = (V3 inX inY inZ), tMin = t0})
            else Just (Intersection {intersectionPoint = ro .+^ (rd ^* t1), intersectionNormal = (V3 outX outY outZ), tMin = t1})
       else Nothing
    
