module Ray
    ( Ray (..)
    , Intersection (..)
    , rayIntersection
    , transformRay
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

rayEpsilon :: (Floating f, Ord f, RealFloat f) => f
rayEpsilon = 1e-5

rayIntersection :: (Epsilon f, Floating f, Ord f, RealFloat f) => Ray f -> Shape f -> Maybe (Intersection f)

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
        b = (temp `dot` rd) * 2.0
        c = temp `dot` temp - sphereRadius * sphereRadius
        disc = b * b - 4.0 * a * c
    in if disc < 0.0
       then Nothing
       else let e = sqrt disc
                denom = 2.0 * a
                t = ((-b) - e) / denom
            in if t > rayEpsilon
               then Just (Intersection {intersectionPoint = ro .+^ (rd ^* t), intersectionNormal = normalize ((temp ^+^ (rd ^* t)) ^/ sphereRadius), tMin = t})
               else let bigT = ((-b) + e) / denom
                    in if bigT > rayEpsilon
                       then Just (Intersection {intersectionPoint = ro .+^ (rd ^* bigT), intersectionNormal = normalize ((temp ^+^ (rd ^* bigT)) ^/ sphereRadius), tMin = bigT})
                       else Nothing

transformRay :: (Epsilon f, Floating f, Ord f, RealFloat f) => M44 f -> M44 f -> Ray f -> Ray f
transformRay worldToView normalMatrix (Ray {rayOrigin = ro, rayDirection = rd}) =
    let (V4 nx ny nz nw) = normalMatrix !* (vector rd)
    in Ray {rayOrigin = P (normalizePoint (worldToView !* (point (unP ro)))), rayDirection = (V3 nx ny nz)}

