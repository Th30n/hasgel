{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Hasgel.GL.Param (
  Param(..), IParam(..), SParam(..)
) where

import Data.Char (chr)
import Foreign (allocaArray, peekArray, peekArray0)

import Graphics.GL.Core45
import Graphics.GL.Types

-- | Single integer parameters.
data IParam =
  MinorVersion
  | MajorVersion
  deriving (Show, Eq)

-- | String parameters.
data SParam =
  Vendor
  | Renderer
  | Version
  | ShadingLanguageVersion
  deriving (Show, Eq)

class Param a b | a -> b where
  getParam :: Param a b => a -> IO b

instance Param IParam Int where
  getParam MinorVersion = getInteger GL_MINOR_VERSION
  getParam MajorVersion = getInteger GL_MAJOR_VERSION

instance Param SParam String where
  getParam Vendor = getString GL_VENDOR
  getParam Renderer = getString GL_RENDERER
  getParam Version = getString GL_VERSION
  getParam ShadingLanguageVersion = getString GL_SHADING_LANGUAGE_VERSION

getInteger :: GLenum -> IO Int
getInteger = fmap head . getIntegerv 1

getIntegerv :: Int -> GLenum -> IO [Int]
getIntegerv n param = allocaArray n $ \dataPtr -> do
    glGetIntegerv param dataPtr
    map fromIntegral <$> peekArray n dataPtr

getString :: GLenum -> IO String
getString name =
  fmap (map (chr . fromIntegral)) . peekArray0 0 =<< glGetString name
