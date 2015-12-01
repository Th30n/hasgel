{-# LANGUAGE OverloadedStrings #-}

module Hasgel.Mesh.OBJ (
  OBJ(..), parseOBJ, parseString, parseFile
) where

import Control.Applicative ((<|>))
import Data.String (IsString (..))

-- Don't handle non ASCII at all.
import Data.Attoparsec.ByteString.Char8 ((<?>))
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Linear as L

type Vertex = L.V3 Float
type UV = L.V2 Float

data OBJ = OBJ
  { objVertices :: [Vertex]
  , objNormals :: [Vertex]
  , objUvs :: [UV]
  , objFaces :: [[(Int, Maybe Int, Maybe Int)]]
  } deriving (Show)

data OBJToken = VertexToken !VertexData | ElementToken !ElementData

data VertexData =
  GeometryVertex !Vertex
  | NormalVertex !Vertex
  | TextureVertex !UV

data ElementData = FaceElement ![(Int, Maybe Int, Maybe Int)] deriving (Show)

emptyOBJ :: OBJ
emptyOBJ = OBJ [] [] [] []

parseFile :: FilePath -> IO (Either String OBJ)
parseFile fp = parseString <$> readFile fp

parseString :: String -> Either String OBJ
parseString = A.parseOnly (parseOBJ <* A.endOfInput) . fromString

parseOBJ :: A.Parser OBJ
parseOBJ = do
  tokens <- A.many1' parseLine
  A.skipSpace
  A.endOfInput
  let obj = foldr joinObj emptyOBJ tokens
      joinObj (VertexToken (GeometryVertex v)) o =
        o { objVertices = v : objVertices o }
      joinObj (VertexToken (NormalVertex v)) o =
        o { objNormals = v : objNormals o }
      joinObj (VertexToken (TextureVertex v)) o =
        o { objUvs = v : objUvs o }
      joinObj (ElementToken (FaceElement f)) o =
        o { objFaces = f : objFaces o }
  pure obj { objVertices = reverse $ objVertices obj,
             objNormals = reverse $ objNormals obj,
             objUvs = reverse $ objUvs obj,
             objFaces = reverse $ objFaces obj }

parseLine :: A.Parser OBJToken
parseLine = do
  A.skipSpace
  A.skipMany (parseComment <* A.skipSpace)
  (parseVertexData <?> "vertex data") <|> parseElementData <?> "element data"

parseComment :: A.Parser ()
parseComment = "#" *> A.skipWhile (A.notInClass "\n\r")

parseVertexData :: A.Parser OBJToken
parseVertexData = do
  vd <- parseGeometryVertex <|> parseNormalVertex <|> parseTextureVertex
  pure $ VertexToken vd

parseGeometryVertex :: A.Parser VertexData
parseGeometryVertex = GeometryVertex <$> ("v " *> parseVertex)

parseNormalVertex :: A.Parser VertexData
parseNormalVertex = NormalVertex <$> ("vn " *> parseVertex)

parseTextureVertex :: A.Parser VertexData
parseTextureVertex = TextureVertex <$> ("vt " *> parseVertex2D)

parseVertex :: A.Parser Vertex
parseVertex = do
  L.V2 x y <- parseVertex2D
  A.skipSpace
  z <- realToFrac <$> A.double
  pure $ L.V3 x y z

parseVertex2D :: A.Parser UV
parseVertex2D = do
  x <- realToFrac <$> A.double
  A.skipSpace
  y <- realToFrac <$> A.double
  pure $ L.V2 x y

parseElementData :: A.Parser OBJToken
parseElementData = ElementToken <$> parseFace

parseFace :: A.Parser ElementData
parseFace = FaceElement <$> ("f " *> A.count 3 (parseFaceVertex <* A.skipSpace))

parseFaceVertex :: A.Parser (Int, Maybe Int, Maybe Int)
parseFaceVertex = do
  v <- A.decimal
  vt <- parseOptionalVertex $ "/" *> A.decimal
  vn <- case vt of
          Nothing -> parseOptionalVertex $ "//" *> A.decimal
          _ -> parseOptionalVertex $ "/" *> A.decimal
  pure (v, vt, vn)
  where parseOptionalVertex p = A.option Nothing $ Just <$> p
