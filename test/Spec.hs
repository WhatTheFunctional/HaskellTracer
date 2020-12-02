import Control.DeepSeq
import Data.List
import Numeric.Limits
import System.IO
import System.Random
import System.Random.Shuffle
import Linear
import Linear.Affine

import Color
import Ray
import Geometry
import Object
import Trace
import Lens
import Sampling
import Camera
import Screen
import Scene
import IO
import Light
import Material
import Shading
import Accelerator
import Mesh

-- Test objects

testSphere :: Shape
testSphere = Sphere (P (V3 0 0 100)) 100

testPlane :: Shape
testPlane = Plane (P (V3 0 0 10)) (V3 0 1 1)

testMissPlane :: Shape
testMissPlane = Plane (P (V3 0 (-10) 0)) (V3 0 1 0)

testHitRay :: Ray
testHitRay = Ray (P (V3 0 0 0)) (V3 0 0 1)

testMissRay :: Ray
testMissRay = Ray (P (V3 0 1000 0)) (V3 0 0 1)

testBlackRGB :: Color Double
testBlackRGB = RGB 0 0 0

testRedRGB :: Color Double
testRedRGB = RGB 1 0 0

testGreenRGB :: Color Double
testGreenRGB = RGB 0 1 0

testPinkRGB :: Color Double
testPinkRGB = RGB 1 0 1

testSkyBlueRGB :: Color Double
testSkyBlueRGB = RGB 0.529 0.808 0.922

testCamera :: Camera
testCamera = Camera (P (V3 0 0 0)) (V3 0 0 1) (V3 0 1 0)

testEmptyKDTree :: KDTree
testEmptyKDTree = buildKDTree defaultTi defaultTt defaultEmptyBonus standardMaxDepth []

testKDTree :: KDTree
testKDTree = buildKDTree defaultTi defaultTt defaultEmptyBonus standardMaxDepth [Object testSphere (ColorMaterial (RGB 1 0 0)) colorShader]

testRandomSpheresKDTree :: KDTree
testRandomSpheresKDTree = 
       let (spheres, g) = randomSpheres 32 (V3 (-300) (-100) (-100)) (V3 300 300 100) 5 30 (mkStdGen 588025)
       in buildKDTree defaultTi defaultTt defaultEmptyBonus standardMaxDepth spheres

testDepthFunction :: Int
testDepthFunction =
       let (spheres, g) = randomSpheres 1000 (V3 (-(300)) (-100) (-100)) (V3 300 300 100) 5 30 (mkStdGen 588025)
       in standardMaxDepth (fromIntegral (length spheres))

-- Test scene

suffernCamera :: Camera
suffernCamera = Camera (P (V3 0 0 100)) (V3 0 0 (-1)) (V3 0 1 0)

suffernSphere0 :: Object
suffernSphere0 = Object (Sphere (P (V3 0 (-25) 0)) 80) (ColorMaterial (RGB 1 0 0)) colorShader

suffernSphere1 :: Object
suffernSphere1 = Object (Sphere (P (V3 0 30 0)) 60) (ColorMaterial (RGB 1 1 0)) colorShader

suffernPlane :: Object
suffernPlane = Object (Plane (P (V3 0 0 0)) (V3 0 1 1)) (ColorMaterial (RGB 0 0.3 0)) colorShader

-- Lit version of Suffern scene

litSuffernCamera :: Camera
litSuffernCamera = cameraLookAt (P (V3 20 50 100)) (V3 0 0 0) (V3 0 1 0)

litSuffernSphere0 :: Object
litSuffernSphere0 = Object (Sphere (P (V3 0 (-25) 0)) 80) (PlasticMaterial (RGB 1 0 0) 1 (RGB 0.8 0 0.8) 1 2.5) phongShader

litSuffernSphere1 :: Object
litSuffernSphere1 = Object (Sphere (P (V3 0 30 0)) 60) (MatteMaterial (RGB 1 1 0) 1) lambertShader

litSuffernPlane :: Object
litSuffernPlane = Object (Plane (P (V3 0 0 0)) (V3 0 1 1)) (MatteMaterial (RGB 0 0.3 0) 1) lambertShader

litSuffernAABB :: Object
litSuffernAABB = Object (AABB identity (V3 (-65) (-65) (-65)) (V3 65 65 65)) (PlasticMaterial (RGB 0.2 0.2 0.5) 1 (RGB 0.4 0.4 0.8) 1 2.5) lambertShader

litSuffernTriangle :: Object
litSuffernTriangle =
    let v0 = (V3 0 0 100)
        v1 = (V3 100 100 0)
        v2 = (V3 (-100) 0 100)
    in Object (Triangle (P v0) (P v1) (P v2) (normalize ((v1 ^-^ v0) `cross` (v2 ^-^ v0)))) (PlasticMaterial (RGB 0.0 0.0 0.8) 1 (RGB 0.0 0.0 0.1) 1 2.5) lambertShader

litSuffernDisk :: Object
litSuffernDisk = Object (Disk (P (V3 0 100 0)) (V3 0 1 0) 40) (PlasticMaterial (RGB 0.0 0.8 0.8) 1 (RGB 0.0 1.0 1.0) 1 2.5) lambertShader

litSuffernRectangle :: Object
litSuffernRectangle = Object (Rectangle (P (V3 (-25) 120 (-25))) (V3 50 0 0) (V3 0 0 50) (V3 0 1 0)) (PlasticMaterial (RGB 0.8 0.0 0.8) 1 (RGB 1.0 0.0 1.0) 1 2.5) lambertShader

suffernLight0 :: Light
suffernLight0 = PointLight (P (V3 80 80 100)) (RGB 1 1 1)

suffernLight1 :: Light
suffernLight1 = PointLight (P (V3 0 (-40) 100)) (RGB 1 0 1)

suffernLight2 :: Light
suffernLight2 = DirectionalLight (V3 1 (-1) (-1)) (RGB 0.5 0.5 0)

suffernLight3 :: Light
suffernLight3 = DirectionalLight (V3 0 (-1) 0) (RGB 1 1 1)

-- Random spheres scene

randomSpheresCamera :: Camera
randomSpheresCamera = cameraLookAt (P (V3 20 200 400)) (V3 0 150 0) (V3 0 1 0)

randomSpheres :: (RandomGen g)
              => Int
              -> V3 Double
              -> V3 Double
              -> Double
              -> Double
              -> g
              -> ([Object], g)
randomSpheres i (V3 minX minY minZ) (V3 maxX maxY maxZ) minR maxR rGenerator 
    = if i <= 0
      then ([], rGenerator)
      else let (x, g0) = randomR (minX, maxX) rGenerator
               (y, g1) = randomR (minY, maxY) g0
               (z, g2) = randomR (minZ, maxZ) g1
               (radius, g3) = randomR (minR, maxR) g2
               (kDR, g4) = randomR (0, 1) g3
               (kDG, g5) = randomR (0, 1) g4
               (kDB, g6) = randomR (0, 1) g5
               (kSR, g7) = randomR (0, 1) g6
               (kSG, g8) = randomR (0, 1) g7
               (kSB, g9) = randomR (0, 1) g8
               (kExp, g10) = randomR (1, 5) g9
               sphere = Object (Sphere (P (V3 x y z)) radius) (PlasticMaterial (RGB kDR kDG kDB) 1 (RGB kSR kSG kSB) 1 kExp) lambertShader
               (spheres, lastG) = randomSpheres (i - 1) (V3 minX minY minZ) (V3 maxX maxY maxZ) minR maxR g10
           in (sphere : spheres, lastG)

randomSpheresPlane :: Object
randomSpheresPlane = Object (Plane (P (V3 0 (-150) 0)) (V3 0 1 0)) (MatteMaterial (RGB 1 1 1) 1) lambertShader

-- Environment light scene

environmentLight0 :: Light
environmentLight0 = EnvironmentLight (RGB 1 1 1)

-- Disk light scene

diskLight0 :: Light
diskLight0 = DiskLight (P (V3 300 300 300)) (V3 (-1) (-1) (-1)) 80 (RGB 0.1 0.5 1)

-- Sphere light scene

sphereLight0 :: Light
sphereLight0 = SphereLight (P (V3 300 300 300)) 80 (RGB 1 0.5 0.1)

-- Rectangle light scene

rectangleLight0 :: Light
rectangleLight0 = RectangleLight (P (V3 300 300 300)) (V3 0 100 0) (V3 0 0 100) (RGB 0.1 1 0.5)

-- Bunny light scene

bunnyLight0 :: Light
bunnyLight0 = RectangleLight (P (V3 300 300 300)) (V3 0 50 0) (V3 0 0 50) (RGB 1 1 1)

bunnyPlane :: Object
bunnyPlane = Object (Plane (P (V3 0 50 0)) (V3 0 1 0)) (MatteMaterial (RGB 1 1 1) 1) lambertShader

bunnyCamera :: Camera
bunnyCamera = cameraLookAt (P (V3 20 300 400)) (V3 0 150 0) (V3 0 1 0)

-- Test functions

testRayIntersectSphere :: IO ()
testRayIntersectSphere = do
    putStrLn "-- Testing Ray Sphere Intersection"
    putStrLn $ show $ rayIntersection testHitRay testSphere

testRayIntersectPlane :: IO ()
testRayIntersectPlane = do
    putStrLn "-- Testing Ray Plane Intersection"
    putStrLn $ show $ rayIntersection testHitRay testPlane

testRayMissSphere :: IO ()
testRayMissSphere = do
    putStrLn "-- Testing Ray Sphere Miss"
    putStrLn $ show $ rayIntersection testMissRay testSphere

testRayMissPlane :: IO ()
testRayMissPlane = do
    putStrLn "-- Testing Ray Plane Miss"
    putStrLn $ show $ rayIntersection testHitRay testMissPlane

testNaiveTraceSphere :: IO ()
testNaiveTraceSphere = do
    putStrLn "-- Testing Naive Trace with a Sphere"
    let ((TraceResult intersection material shader, ray), gen) = traceRays (ListScene [Object testSphere (ColorMaterial testRedRGB) colorShader]) testPinkRGB testHitRay (mkHaltonLDS (mkHaltonCache 15485863 2) 0)
    putStrLn $ show $ (intersection, material)

testNaiveTracePlane :: IO ()
testNaiveTracePlane = do
    putStrLn "-- Testing Naive Trace with a Sphere"
    let ((TraceResult intersection material shader, ray), gen) = traceRays (ListScene [Object testPlane (ColorMaterial testRedRGB) colorShader]) testPinkRGB testHitRay (mkHaltonLDS (mkHaltonCache 15485863 2) 0)
    putStrLn $ show $ (intersection, material)

testRenderBasicSphere :: IO ()
testRenderBasicSphere =
    do putStrLn "-- Writing basic sphere image to basic_sphere.png"
       writePNG "basic_sphere.png"
                (pixelTraceGenerator
                 traceRays
                 traceAllLights
                 (ListScene [Object testSphere (ColorMaterial testRedRGB) colorShader])
                 [EnvironmentLight testBlackRGB]
                 testPinkRGB
                 testCamera
                 (640, 480, 1.0, 2.2)
                 (singleSampling orthoLens)
                 (mkHaltonLDS (mkHaltonCache 15485863 2))
                 (mkStdGen 813580))

testRenderBasicScene :: IO ()
testRenderBasicScene =
    do putStrLn "-- Writing Suffern scene image to suffern_scene.png"
       writePNG "suffern_scene.png"
                (pixelTraceGenerator
                 traceRays
                 traceAllLights
                 (ListScene [suffernSphere0, suffernSphere1, suffernPlane])
                 [EnvironmentLight testBlackRGB]
                 testPinkRGB
                 suffernCamera
                 (200, 200, 1.0, 2.2)
                 (singleSampling orthoLens)
                 (mkHaltonLDS (mkHaltonCache 15485863 2))
                 (mkStdGen 813580))

testRender4xSuperSamplingBasicScene :: IO ()
testRender4xSuperSamplingBasicScene =
    do putStrLn "-- Writing Suffern scene image with 4x supersampling to suffern_scene_4x.png"
       writePNG "suffern_scene_4x.png"
                (pixelTraceGenerator
                 traceRays
                 traceAllLights
                 (ListScene [suffernSphere0, suffernSphere1, suffernPlane])
                 [EnvironmentLight testBlackRGB]
                 testPinkRGB
                 suffernCamera
                 (200, 200, 1.0, 2.2)
                 (grid4xSampling orthoLens)
                 (mkHaltonLDS (mkHaltonCache 15485863 2))
                 (mkStdGen 813580))

testRenderLitScene :: IO ()
testRenderLitScene =
    do putStrLn "-- Writing lit Suffern scene image to lit_suffern_scene.png"
       writePNG "lit_suffern_scene.png"
                (pixelTraceGenerator
                 traceRays
                 traceAllLights
                 (ListScene [litSuffernSphere0, litSuffernSphere1, litSuffernPlane, litSuffernAABB, litSuffernTriangle, litSuffernDisk, litSuffernRectangle])
                 [suffernLight0, suffernLight1, suffernLight2, suffernLight3]
                 testPinkRGB
                 litSuffernCamera
                 (200, 200, 1.0, 2.2)
                 (grid4xSampling orthoLens)
                 (mkHaltonLDS (mkHaltonCache 15485863 2))
                 (mkStdGen 813580))

testRenderRandomSpheresScene :: IO ()
testRenderRandomSpheresScene =
    do putStrLn "-- Writing random spheres scene image to random_spheres_scene.png"
       let (spheres, g) = randomSpheres 1000 (V3 (-300) (-100) (-100)) (V3 300 300 100) 5 10 (mkStdGen 588025)
       writePNG "random_spheres_scene.png"
                (pixelTraceGenerator
                 traceRays
                 traceOneLight
                 --traceAllLights
                 --(ListScene (randomSpheresPlane : spheres))
                 (KDScene (buildKDTree defaultTi defaultTt defaultEmptyBonus standardMaxDepth (randomSpheresPlane : spheres)))
                 --(ListScene spheres)
                 --(KDScene (buildKDTree defaultTi defaultTt defaultEmptyBonus standardMaxDepth spheres))
                 [suffernLight0, suffernLight1, suffernLight2, suffernLight3]
                 testSkyBlueRGB
                 randomSpheresCamera
                 (200, 200, 2.0, 2.2)
                 --(singleSampling (perspectiveLens (pi / 3)))
                 (randomSampling 4 (perspectiveLens (pi / 3)))
                 (mkHaltonLDS (mkHaltonCache 104729 5))
                 --(mkHaltonLDS (mkHaltonCache 20011 5))
                 --(mkHaltonLDS (mkHaltonCache 10007 5))
                 --(mkHaltonLDS (mkHaltonCache 5009 5))
                 --(mkHaltonLDS (mkHaltonCache 1001 5))
                 --(mkHaltonLDS (mkHaltonCache 101 5))
                 (mkStdGen 813580))

testRenderEnvironmentLightScene :: IO ()
testRenderEnvironmentLightScene =
    do putStrLn "-- Writing environment light environment_light_scene.png"
       let (spheres, g) = randomSpheres 1000 (V3 (-300) (-100) (-100)) (V3 300 300 100) 5 30 (mkStdGen 588025)
       writeParallelPNG "environment_light_scene.png"
                (pixelTraceGenerator
                 traceRays
                 traceOneLight
                 (KDScene (buildKDTree defaultTi defaultTt defaultEmptyBonus standardMaxDepth (randomSpheresPlane : spheres)))
                 [environmentLight0]
                 testSkyBlueRGB
                 randomSpheresCamera
                 (640, 480, 2.0, 2.2)
                 (randomSampling 8 (perspectiveLens (pi / 3)))
                 (mkHaltonLDS (mkHaltonCache 1048576 2))
                 (mkStdGen 813580))

testRenderDiskLightScene :: IO ()
testRenderDiskLightScene =
    do putStrLn "-- Writing disk light disk_light_scene.png"
       let (spheres, g) = randomSpheres 1000 (V3 (-300) (-100) (-100)) (V3 300 300 100) 5 30 (mkStdGen 588025)
       writeParallelPNG "disk_light_scene.png"
                (pixelTraceGenerator
                 traceRays
                 traceOneLight
                 (KDScene (buildKDTree defaultTi defaultTt defaultEmptyBonus standardMaxDepth (randomSpheresPlane : spheres)))
                 [diskLight0]
                 testSkyBlueRGB
                 randomSpheresCamera
                 (200, 200, 2.0, 2.2)
                 (randomSampling 8 (perspectiveLens (pi / 3)))
                 (mkHaltonLDS (mkHaltonCache 1048576 2))
                 (mkStdGen 813580))

testRenderSphereLightScene :: IO ()
testRenderSphereLightScene =
    do putStrLn "-- Writing sphere light sphere_light_scene.png"
       let (spheres, g) = randomSpheres 1000 (V3 (-300) (-100) (-100)) (V3 300 300 100) 5 30 (mkStdGen 588025)
       writeParallelPNG "sphere_light_scene.png"
                (pixelTraceGenerator
                 traceRays
                 traceOneLight
                 (KDScene (buildKDTree defaultTi defaultTt defaultEmptyBonus standardMaxDepth (randomSpheresPlane : spheres)))
                 [sphereLight0]
                 testSkyBlueRGB
                 randomSpheresCamera
                 (200, 200, 2.0, 2.2)
                 (randomSampling 8 (perspectiveLens (pi / 3)))
                 (mkHaltonLDS (mkHaltonCache 1048576 2))
                 (mkStdGen 813580))

testRenderRectangleLightScene :: IO ()
testRenderRectangleLightScene =
    do putStrLn "-- Writing rectangle light rectangle_light_scene.png"
       let (spheres, g) = randomSpheres 1000 (V3 (-300) (-100) (-100)) (V3 300 300 100) 5 30 (mkStdGen 588025)
       writeParallelPNG "rectangle_light_scene.png"
                (pixelTraceGenerator
                 traceRays
                 traceOneLight
                 (KDScene (buildKDTree defaultTi defaultTt defaultEmptyBonus standardMaxDepth (randomSpheresPlane : spheres)))
                 [rectangleLight0]
                 testSkyBlueRGB
                 randomSpheresCamera
                 (200, 200, 2.0, 2.2)
                 (randomSampling 8 (perspectiveLens (pi / 3)))
                 (mkHaltonLDS (mkHaltonCache 1048576 2))
                 (mkStdGen 813580))

testRenderBunnyScene :: [Object] -> IO ()
testRenderBunnyScene objects =
    do putStrLn "-- Writing bunny scene to bunny_scene.png"
       let gen0 = mkStdGen 813580
           (cache, count) = mkHaltonCache 1048576 2
           cache' = shuffle' cache 1048576 gen0
       writeParallelPNG "bunny_scene.png"
                (pixelTraceGenerator
                 traceRays
                 traceOneLight
                 (KDScene (buildKDTree defaultTi defaultTt defaultEmptyBonus standardMaxDepth (bunnyPlane : objects)))
                 [bunnyLight0]
                 testSkyBlueRGB
                 bunnyCamera
                 (200, 200, 2.0, 2.2)
                 (randomSampling 8 (perspectiveLens (pi / 3)))
                 (mkHaltonLDS (cache', count))
                 gen0)

printBunnyHeader :: Handle -> Int -> Int -> IO ()
printBunnyHeader handle numVertices numIndices = do
    hPutStrLn handle "ply"
    hPutStrLn handle "format ascii 1.0"
    hPutStrLn handle ("element vertex " ++ (show numVertices))
    hPutStrLn handle "property float x"
    hPutStrLn handle "property float y"
    hPutStrLn handle "property float z"
    hPutStrLn handle ("element edge " ++ (show (numIndices `div` 2)))
    hPutStrLn handle "property int vertex1"
    hPutStrLn handle "property int vertex2"
    hPutStrLn handle "end_header"

printBunnyVertices :: Handle -> [V3 Double] -> IO ()
printBunnyVertices handle [] = return ()
printBunnyVertices handle ((V3 x y z) : vertices) = do
    hPutStrLn handle ((show x) ++ " " ++ (show y) ++ " " ++ (show z))
    printBunnyVertices handle vertices

printBunnyIndices :: Handle -> [Int] -> IO ()
printBunnyIndices handle [] = return ()
printBunnyIndices handle (e0 : e1 : indices) = do
    hPutStrLn handle ((show e0) ++ " " ++ (show e1))
    printBunnyIndices handle indices

testPrintBunnyAABB :: [Object] -> IO ()
testPrintBunnyAABB objects =
    do putStrLn "-- Printing bunny AABB to bunny_aabb.ply"
       let gen0 = mkStdGen 813580
           (cache, count) = mkHaltonCache 1048576 2
           cache' = shuffle' cache 1048576 gen0
           bunnyKDTree = (buildKDTree defaultTi defaultTt defaultEmptyBonus standardMaxDepth (bunnyPlane : objects))
           (vertices, indices) = treeToMesh bunnyKDTree
       withFile "bunny_aabb.ply" WriteMode (\handle -> do
           printBunnyHeader handle (length vertices) (length indices)
           printBunnyVertices handle (reverse vertices)
           printBunnyIndices handle (reverse indices)
           )

runAll :: IO ()
runAll = do putStrLn "Running tests"
            putStrLn "--Suffern camera"
            putStrLn $ show $ suffernCamera
            let ct@(CameraTransforms {w2v = worldToView, nM = normalMatrix}) = (computeCameraTransforms suffernCamera)
                s0@(Object (Sphere (P s0Position) s0Radius) s0Color colorShader) = suffernSphere0
            putStrLn "--Suffern transforms"
            putStrLn $ show $ ct
            putStrLn "--Suffern sphere"
            putStrLn $ show $ s0
            putStrLn "--Suffern sphere view space center"
            putStrLn $ show $ (worldToView !* (point s0Position))
            putStrLn "--Test Empty KD tree"
            putStrLn $ show $ testEmptyKDTree
            putStrLn "--Test KD tree"
            putStrLn $ show $ testKDTree
            putStrLn "--Test KD tree depth function"
            putStrLn $ show $ testDepthFunction
            let getCoord = (\(V3 x _ _) -> x)
                getSplitVector = (\split -> V3 split infinity infinity)
                numObjects = 32
                (spheres, g) = randomSpheres numObjects (V3 (-300) (-100) (-100)) (V3 300 300 100) 5 30 (mkStdGen 588025)
                treeAABB = foldr (\aabb accumulatorAABB -> 
                                      case mergeBoundingBoxes aabb accumulatorAABB of
                                          Nothing -> accumulatorAABB
                                          Just mergedAABB -> mergedAABB) (AABB identity (V3 infinity infinity infinity) (V3 (-infinity) (-infinity) (-infinity))) (fmap (\(Object shape _ _) -> getShapeBoundingBox shape) spheres)
                shapeBoundingBoxes = fmap (\(Object shape _ _) -> getShapeBoundingBox shape) spheres
                getSplits = (\objects -> foldr (\(AABB _ v0 v1) accumulator -> (getCoord v0) : (getCoord v1) : accumulator) [] objects)
                splits = fmap getSplitVector (sort (getSplits shapeBoundingBoxes))
                splitIndices = foldr (\index accumulator -> index : (index + 1) : accumulator) [] [0..(numObjects - 1)]
                splitsAndIndices = zip splits splitIndices

                bonusFunction = (\index -> if index == 0 || index == numObjects then emptyBonus else 0)
                invAABBSurfaceArea = 1 / (boundingBoxSurfaceArea treeAABB)
                ti = defaultTi
                tt = defaultTt
                emptyBonus = defaultEmptyBonus
                noSplitCost = ti * (fromIntegral numObjects)
                splitCosts = fmap (\(split, index) ->
                                       let (leftAABB, rightAABB) = splitBoundingBox split treeAABB
                                           bonusFactor = (1 - (bonusFunction index))
                                           leftFactor = (fromIntegral index) * (boundingBoxSurfaceArea leftAABB) * invAABBSurfaceArea
                                           rightFactor = (fromIntegral (numObjects - index)) * (boundingBoxSurfaceArea rightAABB) * invAABBSurfaceArea
                                           splitCost = tt + ti * bonusFactor * (leftFactor + rightFactor)
                                       in (splitCost, index)) splitsAndIndices
                result = foldr (\(split, index) (minSplit, minSplitCost, minLeftAABB, minRightAABB) ->
                                 let (leftAABB, rightAABB) = splitBoundingBox split treeAABB
                                     bonusFactor = (1 - (bonusFunction index))
                                     leftFactor = (fromIntegral index) * (boundingBoxSurfaceArea leftAABB) * invAABBSurfaceArea
                                     rightFactor = (fromIntegral (numObjects - index)) * (boundingBoxSurfaceArea rightAABB) * invAABBSurfaceArea
                                     splitCost = tt + ti * bonusFactor * (leftFactor + rightFactor)
                                 in if splitCost < minSplitCost
                                    then (split, splitCost, leftAABB, rightAABB)
                                    else (minSplit, minSplitCost, minLeftAABB, minRightAABB)) (V3 infinity infinity infinity, noSplitCost, treeAABB, treeAABB) splitsAndIndices
            putStrLn "--Test no split cost"
            putStrLn $ show $ noSplitCost
            putStrLn "--Test split costs"
            putStrLn $ show $ splitCosts
            putStrLn "--Test splits and indices"
            putStrLn $ show $ splitsAndIndices
            putStrLn "--Test split result"
            putStrLn $ show $ result
            putStrLn "--Test Spheres KD tree"
            putStrLn $ show $ testRandomSpheresKDTree
            let haltonLDS = mkHaltonLDS (mkHaltonCache 100 2) 0
            putStrLn "--Test Halton sample"
            putStrLn $ show $ sampleR (5, 10) haltonLDS
            putStrLn "--Test Halton rectangle"
            putStrLn $ show $ sampleRectangle 20 25 haltonLDS
            putStrLn "--Test Halton disk"
            putStrLn $ show $ sampleDisk 32 haltonLDS
            putStrLn "--Test Halton hemisphere"
            putStrLn $ show $ (sampleHemisphere haltonLDS :: ((Double, Double), Halton))
            putStrLn "--Test Halton sphere"
            putStrLn $ show $ (sampleSphere haltonLDS :: ((Double, Double), Halton))
            putStrLn $ show $ [(x, y) | x <- [0..10], y <- [0..10]]
            putStrLn "--Test loading bunny"
            mesh <- loadMeshPLY identity (ColorMaterial testRedRGB) colorShader "bun_zipper_res4.ply"
            putStrLn $ show $ fmap (\(plyHeader, m) -> take 20 m) mesh
            testRayIntersectSphere
            testRayIntersectPlane
            testRayMissSphere
            testRayMissPlane
            testNaiveTraceSphere
            testNaiveTracePlane
            testRenderBasicSphere
            testRenderBasicScene
            testRender4xSuperSamplingBasicScene
            testRenderLitScene
            testRenderRandomSpheresScene
            testRenderEnvironmentLightScene 
            testRenderDiskLightScene 
            testRenderSphereLightScene 
            testRenderRectangleLightScene 

runJustRandomSpheres :: IO ()
runJustRandomSpheres = testRenderRandomSpheresScene

runJustEnvironmentLight :: IO ()
runJustEnvironmentLight = testRenderEnvironmentLightScene 

runJustDiskLight :: IO ()
runJustDiskLight = testRenderDiskLightScene 

runJustSphereLight :: IO ()
runJustSphereLight = testRenderSphereLightScene 

runJustRectangleLight :: IO ()
runJustRectangleLight = testRenderRectangleLightScene 

runJustRenderBunny :: IO ()
runJustRenderBunny =
    do let s = 2000
           transform = (V4 (V4 s 0 0 0)
                           (V4 0 s 0 0)
                           (V4 0 0 s 0)
                           (V4 0 0 0 1))
       mesh <- loadMeshPLY transform
                           (PlasticMaterial (RGB 0.2 0.2 0.5) 1 (RGB 0.4 0.4 0.8) 1 2.5)
                           lambertShader
                           "bun_zipper_res2.ply"
       case mesh of
           Nothing -> return ()
           Just (plyHeader, objects) ->
               do putStrLn $ show $ plyHeader
                  putStrLn $ ("Mesh size: " ++ (show $ length objects))
                  testRenderBunnyScene objects

runPrintBunnyAABB :: IO ()
runPrintBunnyAABB =
    do let s = 1
           transform = (V4 (V4 s 0 0 0)
                           (V4 0 s 0 0)
                           (V4 0 0 s 0)
                           (V4 0 0 0 1))
       mesh <- loadMeshPLY transform
                           (PlasticMaterial (RGB 0.2 0.2 0.5) 1 (RGB 0.4 0.4 0.8) 1 2.5)
                           lambertShader
                           "bun_zipper_res2.ply"
       case mesh of
           Nothing -> return ()
           Just (plyHeader, objects) ->
               do putStrLn $ show $ plyHeader
                  putStrLn $ ("Mesh size: " ++ (show $ length objects))
                  testPrintBunnyAABB objects

main :: IO ()
--main = runAll
--main = runJustRandomSpheres
--main = runJustEnvironmentLight
--main = runJustDiskLight
--main = runJustSphereLight
--main = runJustRectangleLight
--main = runJustRenderBunny
main = runPrintBunnyAABB
