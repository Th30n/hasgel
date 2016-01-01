module Hasgel.Drawable (
  Drawable(draw), freeDrawable, mesh2Drawable
) where

import Foreign (nullPtr)

import qualified Hasgel.GL as GL
import Hasgel.Mesh

import Graphics.GL.Core45

data Drawable = Drawable
  { dVao :: GL.VertexArray
  , draw :: IO ()
  }

freeDrawable :: Drawable -> IO ()
freeDrawable = GL.delete . dVao

mesh2Drawable :: Mesh -> IO Drawable
mesh2Drawable mesh = do
  vao <- (GL.gen >>=) $ flip GL.setVertexArray $ do
    GL.attrib $ meshVertices mesh
    GL.attrib $ meshNormals mesh
    GL.attrib $ meshUvs mesh
    GL.indexBuffer $ meshVertexIx mesh
  let vertexCount = meshVertexCount mesh
  pure Drawable {
    dVao = vao,
    draw = do
        GL.bindVertexArray vao
        GL.drawElements GL_TRIANGLES vertexCount GL_UNSIGNED_SHORT nullPtr
    }

