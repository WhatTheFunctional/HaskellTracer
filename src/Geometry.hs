module Geometry
    ( Ray (..)
    , Object (..)
    , rayIntersection
    ) where

import Linear
import Linear.Epsilon
import Linear.Vector
import Linear.Affine
import Linear.Metric

data Ray a = Ray { rayOrigin :: Point V3 a
                 , rayDirection :: V3 a
                 } deriving (Show, Eq)

data Object a = Plane (Point V3 a) (V3 a) -- Point and normal
              | Sphere (Point V3 a) a -- Origin and radius
              deriving (Show, Eq)

data Intersection a = Intersection { intersectionPoint :: Point V3 a
                                   , intersectionNormal :: V3 a
                                   , tMin :: a
                                   } deriving (Show, Eq)

rayIntersection :: (Ord a, Epsilon a, Floating a) => Ray a -> Object a -> Maybe (Intersection a)
rayIntersection (Ray {rayOrigin = ro, rayDirection = rd}) (Plane planePoint planeNormal) = 
    let t = (planePoint .-. ro) `dot` (planeNormal ^/ (rd `dot` planeNormal))
    in if (nearZero t)
       then Nothing
       else Just (Intersection {intersectionPoint = ro .+^ (rd ^* t), intersectionNormal = planeNormal, tMin = t})
rayIntersection (Ray {rayOrigin = ro, rayDirection = rd}) (Sphere sphereOrigin sphereRadius) =
    let co = ro .-. sphereOrigin
        a = rd `dot` rd
        b = (co `dot` rd) * 2.0
        c = co `dot` co - sphereRadius * sphereRadius
        disc = b * b - 4.0 * a * c
    in if disc < 0.0
       then Nothing
       else let e = sqrt disc
                denom = 2.0 * a
                t = (-(b + e)) / denom
            in if (nearZero t)
               then Nothing
               else if t > 0.0
                    then Just (Intersection {intersectionPoint = ro .+^ (rd ^* t), intersectionNormal = co ^+^ (rd ^* (t / sphereRadius)), tMin = t})
                    else let bigT = (e - b) / denom
                         in if (nearZero bigT)
                            then Nothing
                            else if bigT > 0.0
                                 then Just (Intersection {intersectionPoint = ro .+^ (rd ^* bigT), intersectionNormal = co ^+^ (rd ^* (bigT / sphereRadius)), tMin = bigT})
                                 else Nothing

