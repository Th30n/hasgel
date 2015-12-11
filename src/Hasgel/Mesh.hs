module Hasgel.Mesh (
  Face(..), Mesh(..), meshVertexCount, cube, loadHmd, loadObj
) where

import Control.Monad (replicateM)
import Data.Binary (Binary (..), decodeFileOrFail, getWord8, putWord8)
import Data.Char (ord)
import Data.List (findIndex, nub, sort)
import Data.Word (Word16)

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

data Face = Face
  { faceVertexIx :: [Word16]
  , faceUvIx :: Maybe [Word16]
  , faceNormalIx :: Maybe [Word16]
  } deriving (Show)

instance Binary Face where
  put face = do
    put $ faceVertexIx face
    put $ faceUvIx face
    put $ faceNormalIx face

  get = Face <$> get <*> get <*> get

meshVertexCount :: Mesh -> Int
meshVertexCount = (3 *) . length . meshVertices

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
obj2Mesh (OBJ verts normals uvs faces) =
  rearrangeMesh $ Mesh verts normals (Just uvs) $ convertFace <$> faces
  where convertFace ((av, avt, avn), (bv, bvt, bvn), (cv, cvt, cvn)) =
          Face [av, bv, cv] (sequence [avt, bvt, cvt]) $ sequence [avn, bvn, cvn]

-- | Reorder vertices and normals and adjust face vertex indices so that they
-- can be indexed in OpenGL.
rearrangeMesh :: Mesh -> Mesh
rearrangeMesh mesh =
  let no = normalOrder $ meshFaces mesh
      vs = reorderVertices (meshVertices mesh) $ map fst no
      norms = reorderVertices (meshNormals mesh) $ map snd no
      faces = flip reorderFaceIxs no <$> (meshFaces mesh)
  in Mesh vs norms Nothing faces

cube :: Mesh
cube = rearrangeMesh cube'

cube' :: Mesh
cube' = Mesh {
  meshVertices = [ L.V3 (-1) (-1)   1,
                   L.V3 (-1) (-1) (-1),
                   L.V3   1  (-1) (-1),
                   L.V3   1  (-1)   1,
                   L.V3 (-1)   1    1,
                   L.V3 (-1)   1  (-1),
                   L.V3   1    1  (-1),
                   L.V3   1    1    1
                 ]
  , meshFaces = [ Face [2, 3, 4] Nothing $ Just [1, 1, 1],
                  Face [8, 7, 6] Nothing $ Just [2, 2, 2],
                  Face [5, 6, 2] Nothing $ Just [3, 3, 3],
                  Face [6, 7, 3] Nothing $ Just [4, 4, 4],
                  Face [3, 7, 8] Nothing $ Just [5, 5, 5],
                  Face [1, 4, 8] Nothing $ Just [6, 6, 6],
                  Face [1, 2, 4] Nothing $ Just [1, 1, 1],
                  Face [5, 8, 6] Nothing $ Just [2, 2, 2],
                  Face [1, 5, 2] Nothing $ Just [3, 3, 3],
                  Face [2, 6, 3] Nothing $ Just [4, 4, 4],
                  Face [4, 3, 8] Nothing $ Just [5, 5, 5],
                  Face [5, 1, 8] Nothing $ Just [6, 6, 6]
                ]
  , meshNormals = [ L.V3   0 (-1)  0,
                    L.V3   0   1   0,
                    L.V3 (-1)  0   0,
                    L.V3   0   0 (-1),
                    L.V3   1   0   0,
                    L.V3   0   0   1
                  ]
  , meshUvs = Nothing
  }

normalOrder :: [Face] -> [(Word16, Word16)]
normalOrder = nub . sort . go
  where go [] = []
        go ((Face _ _ Nothing):fs) = go fs
        go ((Face vs _ (Just ns)):fs) = zip vs ns ++ go fs

reorderVertices :: [a] -> [Word16] -> [a]
reorderVertices vs = go
  where go [] = []
        go (i:ixs) = (vs !! (fromIntegral i - 1)):(go ixs)

reorderFaceIxs :: Face -> [(Word16, Word16)] -> Face
reorderFaceIxs f@(Face vs _ (Just ns)) ixs =
  f { faceVertexIx = fst $ go vs ns,
      faceNormalIx = Just . snd $ go vs ns }
  where go [] _ = ([], [])
        go _ [] = ([], [])
        go (v:tv) (n:tn)
          | Just i <- findIndex (== (v, n)) ixs =
              let (vs', ns') = go tv tn
              in (1 + fromIntegral i:vs', 1 + fromIntegral i:ns')
          | otherwise = let (vs', ns') = go tv tn
                        in (v:vs', n:ns')
reorderFaceIxs f _ = f

