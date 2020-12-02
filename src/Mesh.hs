module Mesh
    ( 
    loadMeshPLY
    ) where

import Control.Exception
import Data.List
import Data.List.Split
import Data.String.Utils
import qualified Data.Vector as Vec
import Linear
import Linear.Affine
import Linear.Metric
import System.IO

import Color
import Geometry
import Material
import Object
import Shading

data PLYException = VertexException String | FaceException String deriving (Show)

instance Exception PLYException

loadHeader :: Handle -> (Maybe Int) -> (Maybe Int) -> IO (Maybe (Int, Int))
loadHeader handle Nothing Nothing = do
    line <- hGetLine handle
    let sline = strip line
    case stripPrefix "element vertex " sline of
        Just numVertices -> loadHeader handle (Just (read numVertices)) Nothing
        Nothing -> 
            case stripPrefix "element face " sline of
                Just numFaces -> loadHeader handle Nothing (Just (read numFaces))
                Nothing -> loadHeader handle Nothing Nothing
loadHeader handle (Just numVertices) Nothing = do
    line <- hGetLine handle
    let sline = strip line
    case stripPrefix "element face " sline of
        Just numFaces -> loadHeader handle (Just numVertices) (Just (read numFaces))
        Nothing -> loadHeader handle (Just numVertices) Nothing
loadHeader handle Nothing (Just numFaces) = do
    line <- hGetLine handle
    let sline = strip line
    case stripPrefix "element vertex " sline of
        Just numVertices -> loadHeader handle (Just (read numVertices)) (Just numFaces)
        Nothing -> loadHeader handle Nothing (Just numFaces)
loadHeader handle (Just numVertices) (Just numFaces) = do
    line <- hGetLine handle
    let sline = strip line
    if sline == "end_header" then
        return (Just (numVertices, numFaces))
    else loadHeader handle (Just numVertices) (Just numFaces)

loadVertices :: Handle -> Int -> Int -> [V3 Double] -> IO [V3 Double]
loadVertices handle currentVertex numVertices vertices
    | currentVertex == numVertices = return vertices
    | otherwise = do
        line <- hGetLine handle
        let sline = strip line
        let vertex = splitOn " " sline
        case vertex of
            [x, y, z] -> loadVertices handle (currentVertex + 1) numVertices ((V3 (read x) (read y) (read z)) : vertices)
            [x, y, z, confidence, intensity] -> loadVertices handle (currentVertex + 1) numVertices ((V3 (read x) (read y) (read z)) : vertices)
            _ -> throw (VertexException line)

loadFaces :: Handle -> Int -> Int -> [Int] -> IO [Int]
loadFaces handle currentFace numFaces faces
    | currentFace == numFaces = return faces
    | otherwise = do
        line <- hGetLine handle
        let sline = strip line
        let face = splitOn " " sline
        case face of
            [f, v0, v1, v2] -> loadFaces handle (currentFace + 1) numFaces ((read v2) : (read v1) : (read v0) : faces)
            _ ->  throw (FaceException line)

makeTriangles :: Int -> Int -> (Vec.Vector (V3 Double)) -> (Vec.Vector Int) -> [Shape] -> [Shape]
makeTriangles currentFace numFaces vertices faces triangles
    | currentFace == numFaces = triangles
    | otherwise =
        let v0 = vertices Vec.! (faces Vec.! (currentFace * 3))
            v1 = vertices Vec.! (faces Vec.! (currentFace * 3 + 1))
            v2 = vertices Vec.! (faces Vec.! (currentFace * 3 + 2))
            n = signorm $ (v1 - v0) `cross` (v2 - v0)
        in makeTriangles (currentFace + 1) numFaces vertices faces ((Triangle (P v0) (P v1) (P v2) n) : triangles)

loadMeshPLY :: M44 Double -> Material -> (ShadePoint -> Color Double) -> String -> IO (Maybe ((Int, Int), [Object]))
loadMeshPLY transform material shader filename =
    withFile filename ReadMode (\handle -> do
        header <- loadHeader handle Nothing Nothing
        case header of
            Nothing -> return Nothing
            Just (numVertices, numFaces) -> do
                vertices <- loadVertices handle 0 numVertices [] 
                let verticesVec = Vec.fromList $ reverse $ vertices
                faces <- loadFaces handle 0 numFaces []
                let facesVec = Vec.fromList $ reverse $ faces
                let triangles = makeTriangles 0 numFaces verticesVec facesVec []
                let objects = fmap (\triangle -> Object (transformShape transform identity triangle) material shader) triangles
                return (Just ((numVertices, numFaces), objects))
        )
