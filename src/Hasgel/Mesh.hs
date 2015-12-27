module Hasgel.Mesh (
  Mesh(..), meshVertexCount, meshVertexIx,
  cube, loadHmd, loadObj,
  -- For benchmarks
  obj2Mesh
) where

import Control.Monad (replicateM)
import Data.Array (Array, (!))
import qualified Data.Array as A
import Data.Binary (Binary (..), decodeFileOrFail, getWord8, putWord8)
import Data.Char (ord)
import Data.List (group, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Word (Word16)

import Control.DeepSeq (NFData (..))
import qualified Linear as L

import Hasgel.Mesh.OBJ (OBJ (..))
import qualified Hasgel.Mesh.OBJ as OBJ

data Mesh = Mesh
  { meshVertices :: [L.V3 Float]
  , meshNormals :: [L.V3 Float]
  , meshUvs :: Maybe [L.V2 Float]
  , meshFaces :: [Face]
  } deriving (Show)

instance Binary Mesh where
  put mesh = do
    let magic = map (fromIntegral . ord) "HMD1"
    sequence_ $ putWord8 <$> magic
    put $ meshVertices mesh
    put $ meshNormals mesh
    put $ meshUvs mesh
    put $ meshFaces mesh

  get = do
    magic <- replicateM 4 getWord8
    if magic /= map (fromIntegral . ord) "HMD1"
      then fail "Invalid file format"
      else Mesh <$> get <*> get <*> get <*> get

data Face = Face { faceVertexIx :: [Word16] } deriving (Show)

instance Binary Face where
  put (Face ixs) = put ixs
  get = Face <$> get

instance NFData Mesh where
  rnf mesh = rnf (meshVertices mesh) `seq`
             rnf (meshNormals mesh) `seq`
             rnf (meshUvs mesh) `seq`
             rnf (meshFaces mesh)

instance NFData Face where
  rnf = rnf . faceVertexIx

newtype FaceDesc = FaceDesc ([Word16], Maybe [Word16], Maybe [Word16])

meshVertexCount :: Mesh -> Int
meshVertexCount = (3 *) . length . meshVertices

meshVertexIx :: Mesh -> [Word16]
meshVertexIx = concatMap faceVertexIx . meshFaces

-- | Load the subset of Wavefront OBJ format.
loadObj :: FilePath -> IO (Either String Mesh)
loadObj fp = fmap obj2Mesh <$> OBJ.parseFile fp

-- | Load custom model format. This uses the 'Binary' instance of 'Mesh'.
loadHmd :: FilePath -> IO (Either String Mesh)
loadHmd fp = do
  eitherMesh <- decodeFileOrFail fp
  pure $ case eitherMesh of
           Left (_, err) -> Left err
           Right m -> Right m

obj2Mesh :: OBJ -> Mesh
obj2Mesh (OBJ verts normals _ faces) =
  rearrangeMesh (convertFace <$> faces) verts normals
  where convertFace ((av, avt, avn), (bv, bvt, bvn), (cv, cvt, cvn)) =
          FaceDesc ([av, bv, cv], sequence [avt, bvt, cvt],
                    sequence [avn, bvn, cvn])

-- | Reorder vertices and normals and adjust face vertex indices so that they
-- can be indexed in OpenGL.
rearrangeMesh :: [FaceDesc] -> [L.V3 Float] -> [L.V3 Float]  -> Mesh
rearrangeMesh faces vertices normals =
  let no = normalOrder faces
      len = length vertices - 1
      normLen = length normals - 1
      arrayVertices = A.listArray (0, len) vertices
      vs = reorderVertices arrayVertices $ map fst no
      arrayNormals = A.listArray (0, normLen) normals
      norms = reorderVertices arrayNormals $ map snd no
      ixsMap = M.fromList $ zip no [0..]
      fs = flip reorderFaceIxs ixsMap <$> faces
  in Mesh vs norms Nothing fs

cube :: Mesh
cube = (\(vs, ns, fs) -> rearrangeMesh fs vs ns) cube'

cube' :: ([L.V3 Float], [L.V3 Float], [FaceDesc])
cube' = (
  -- vertices
  [L.V3 (-1) (-1)   1,
   L.V3 (-1) (-1) (-1),
   L.V3   1  (-1) (-1),
   L.V3   1  (-1)   1,
   L.V3 (-1)   1    1,
   L.V3 (-1)   1  (-1),
   L.V3   1    1  (-1),
   L.V3   1    1    1
  ],
  -- normals
  [L.V3   0 (-1)  0,
   L.V3   0   1   0,
   L.V3 (-1)  0   0,
   L.V3   0   0 (-1),
   L.V3   1   0   0,
   L.V3   0   0   1
  ],
  -- faces
  [FaceDesc ([2, 3, 4], Nothing, Just [1, 1, 1]),
   FaceDesc ([8, 7, 6], Nothing, Just [2, 2, 2]),
   FaceDesc ([5, 6, 2], Nothing, Just [3, 3, 3]),
   FaceDesc ([6, 7, 3], Nothing, Just [4, 4, 4]),
   FaceDesc ([3, 7, 8], Nothing, Just [5, 5, 5]),
   FaceDesc ([1, 4, 8], Nothing, Just [6, 6, 6]),
   FaceDesc ([1, 2, 4], Nothing, Just [1, 1, 1]),
   FaceDesc ([5, 8, 6], Nothing, Just [2, 2, 2]),
   FaceDesc ([1, 5, 2], Nothing, Just [3, 3, 3]),
   FaceDesc ([2, 6, 3], Nothing, Just [4, 4, 4]),
   FaceDesc ([4, 3, 8], Nothing, Just [5, 5, 5]),
   FaceDesc ([5, 1, 8], Nothing, Just [6, 6, 6])
  ])

normalOrder :: [FaceDesc] -> [(Word16, Word16)]
normalOrder = nubSorted . sort . go
  where go [] = []
        go (FaceDesc (_, _, Nothing) : fs) = go fs
        go (FaceDesc (vs, _, Just ns) : fs) = zip vs ns ++ go fs
        nubSorted = map head . group

reorderVertices :: Array Int a -> [Word16] -> [a]
reorderVertices vs = map (\i -> vs ! (fromIntegral i - 1))

reorderFaceIxs :: FaceDesc -> Map (Word16, Word16) Word16 -> Face
reorderFaceIxs (FaceDesc (vs, _, Just ns)) ixs =
  Face $ go vs ns
  where go [] _ = []
        go _ [] = []
        go (v:tv) (n:tn) = let i = M.findWithDefault (v - 1) (v, n) ixs
                           in i : go tv tn
reorderFaceIxs (FaceDesc (vs, _, _)) _ = Face vs
