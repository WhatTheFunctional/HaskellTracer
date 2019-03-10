module Ray
    ( Ray (..)
    , Intersection (..)
    , rayIntersection
    ) where

import Numeric.Limits
import Linear
import Linear.Affine
import Linear.Metric
import Linear.Matrix

import Geometry

data Ray = Ray { rayOrigin :: Point V3 Double
               , rayDirection :: V3 Double
               }
         deriving (Show, Eq)

data Intersection = Intersection { intersectionPoint :: Point V3 Double
                                 , intersectionNormal :: V3 Double
                                 , tMin :: Double
                                 }
                  deriving (Show, Eq)

rayEpsilon :: Double
rayEpsilon = 1e-5

rayIntersection :: Ray -> Shape -> Maybe Intersection

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

rayIntersection (Ray {rayOrigin = ro, rayDirection = rd}) (Triangle (P v0) (P v1) (P v2) n) =
    let aei = v0 ^-^ v1
        bfj = v0 ^-^ v2
        dhl = v0 ^-^ unP ro
        m = transpose (V3 aei bfj rd)
    in if det33 m == 0
       then Nothing
       else let (V3 beta gamma t) = (inv33 m) !* dhl
            in if (beta < 0) || (gamma < 0) || (beta + gamma > 1)
               then Nothing
               else if t < rayEpsilon
                    then Nothing
                    else Just (Intersection {intersectionPoint = ro .+^ (rd ^* t), intersectionNormal = n, tMin = t})
    
rayIntersection (Ray {rayOrigin = ro, rayDirection = rd}) (Disk c n r) =
    let t = (c .-. ro) `dot` (n ^/ (rd `dot` n))
    in if t < rayEpsilon
       then Nothing
       else let p = ro .+^ (rd ^* t)
                dSquared = qdA c p
            in if dSquared >= r * r
               then Nothing
               else Just (Intersection {intersectionPoint = p, intersectionNormal = n, tMin = t})

rayIntersection (Ray {rayOrigin = ro, rayDirection = rd}) (Rectangle c e0 e1 n) =
    let t = (c .-. ro) `dot` (n ^/ (rd `dot` n))
    in if t <= rayEpsilon
       then Nothing
       else let p = ro .+^ (rd ^* t)
                d = p .-. c
                ddote0 = d `dot` e0
                ddote1 = d `dot` e1
                lenE0Squared = e0 `dot` e0
                lenE1Squared = e1 `dot` e1
            in if (ddote0 < 0) || (ddote0 > lenE0Squared) || (ddote1 < 0) || (ddote1 > lenE1Squared)
               then Nothing
               else Just (Intersection {intersectionPoint = p, intersectionNormal = n, tMin = t})

