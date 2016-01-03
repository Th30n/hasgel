module Hasgel.Drawable (
  Drawable(..), createPointDrawable, mesh2Drawable
) where

import Foreign (nullPtr)

import qualified Hasgel.GL as GL
import Hasgel.Mesh

import Graphics.GL.Core45

data Drawable = Drawable
  { freeDrawable :: IO ()
  , draw :: IO ()
  }

createPointDrawable :: IO Drawable
createPointDrawable = do
  vao <- (GL.gen >>=) $ flip GL.setVertexArray $
    GL.attrib3f 0 0 0
  pure Drawable {
    freeDrawable = GL.delete vao,
    draw = do
        GL.bindVertexArray vao
        glDrawArrays GL_POINTS 0 1
    }

mesh2Drawable :: Mesh -> IO Drawable
mesh2Drawable mesh = do
  vao <- (GL.gen >>=) $ flip GL.setVertexArray $ do
    GL.attrib $ meshVertices mesh
    GL.attrib $ meshNormals mesh
    GL.attrib $ meshUvs mesh
    GL.indexBuffer $ meshVertexIx mesh
  let vertexCount = meshVertexCount mesh
  pure Drawable {
    freeDrawable = GL.delete vao,
    draw = do
        GL.bindVertexArray vao
        GL.drawElements GL_TRIANGLES vertexCount GL_UNSIGNED_SHORT nullPtr
    }
