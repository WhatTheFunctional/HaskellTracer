module Geometry
    ( Ray (..)
    , Shape (..)
    , Intersection (..)
    , rayIntersection
    ) where

import Linear
import Linear.Epsilon
import Linear.Vector
import Linear.Affine
import Linear.Metric

data Ray f = Ray { rayOrigin :: Point V3 f
                 , rayDirection :: V3 f
                 } deriving (Show, Eq)

data Shape f = Plane (Point V3 f) (V3 f) -- Point and normal
             | Sphere (Point V3 f) f -- Origin and radius
             deriving (Show, Eq)

data Intersection f = Intersection { intersectionPoint :: Point V3 f
                                   , intersectionNormal :: V3 f
                                   , tMin :: f
                                   } deriving (Show, Eq)

rayIntersection :: (Ord f, Epsilon f, Floating f) => Ray f -> Shape f -> Maybe (Intersection f)

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
                    then Just (Intersection {intersectionPoint = ro .+^ (rd ^* t), intersectionNormal = normalize (co ^+^ (rd ^* (t / sphereRadius))), tMin = t})
                    else let bigT = (e - b) / denom
                         in if (nearZero bigT)
                            then Nothing
                            else if bigT > 0.0
                                 then Just (Intersection {intersectionPoint = ro .+^ (rd ^* bigT), intersectionNormal = normalize (co ^+^ (rd ^* (bigT / sphereRadius))), tMin = bigT})
                                 else Nothing

