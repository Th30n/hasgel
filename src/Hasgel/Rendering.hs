{-# LANGUAGE FlexibleContexts #-}

module Hasgel.Rendering (
  renderPlayer, renderPlayerShots, renderInvaders, axisRenderer
) where

import Control.Monad (when)
import System.Environment (getArgs)
import Foreign (nullPtr)

import Control.Monad.Base (MonadBase (..))
import Control.Monad.State (MonadState(..), gets)
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Control.Lens ((^.))
import Graphics.GL.Core45
import Linear ((!*!))
import qualified Linear as L

import Hasgel.Simulation (Simulation(..), HasSimulation(..))
import Hasgel.Game (GameState(..), Player(..))
import Hasgel.Transform(Transform(..), transform2M44, deg2Rad)
import Hasgel.GL (ShaderType(..), Program, useProgram, drawElements,
                  uniform, getUniformLocation)
import Hasgel.Mesh (Mesh, meshVertexCount)
import Hasgel.Resources (HasResources(..), Resources(..))
import qualified Hasgel.Resources as Res

mainProgramDesc :: Res.ProgramDesc
mainProgramDesc = [("shaders/basic.vert", VertexShader),
                   ("shaders/basic.frag", FragmentShader)]

normalsProgramDesc :: Res.ProgramDesc
normalsProgramDesc = [("shaders/basic.vert", VertexShader),
                      ("shaders/normals.geom", GeometryShader),
                      ("shaders/color.frag", FragmentShader)]

axisProgramDesc :: Res.ProgramDesc
axisProgramDesc = [("shaders/axis.vert", VertexShader),
                   ("shaders/axis.geom", GeometryShader),
                   ("shaders/color.frag", FragmentShader)]

persp :: L.M44 Float
persp = L.perspective fovy ar n f
        where fovy = deg2Rad 60
              ar = 800 / 600
              n = 0.1
              f = 100

camera :: L.M44 Float
camera = L.lookAt eye center up
  where eye = L.V3 0 10 21
        center = L.V3 0 (eye ^. L._y) 0
        up = L.V3 0 1 0

renderPlayerShots :: (HasResources s, HasSimulation s GameState,
                      MonadBaseControl IO m, MonadState s m) =>
                     m ()
renderPlayerShots = do
  playerShots <- gets $ gPlayerShots . simState . getSimulation
  mapM_ cubeRenderer playerShots

renderInvaders :: (HasResources s, HasSimulation s GameState,
                   MonadBaseControl IO m, MonadState s m) =>
                  m ()
renderInvaders = do
  invaders <- gets $ gInvaders . simState . getSimulation
  mapM_ cubeRenderer invaders

renderPlayer :: (HasResources s, HasSimulation s GameState,
                 MonadBaseControl IO m, MonadState s m) =>
                m ()
renderPlayer = cubeRenderer =<< gets getPlayerTransform

cubeRenderer :: (HasResources s, HasSimulation s GameState,
                 MonadBaseControl IO m, MonadState s m) =>
                Transform -> m ()
cubeRenderer transform = do
  mainProg <- Res.loadProgram mainProgramDesc
  normalsProg <- Res.loadProgram normalsProgramDesc
  res <- gets Res.getResources
  let renderCube = renderMesh (resMesh res) transform
  liftBase $ do
    renderCube mainProg
    args <- getArgs
    when ("-normals" `elem` args) $ renderCube normalsProg

renderMesh :: Mesh -> Transform -> Program -> IO ()
renderMesh mesh transform prog = do
  useProgram prog
  uniformProjection prog
  let vertexCount = meshVertexCount mesh
      model = transform2M44 transform
  setModelTransform prog model
  drawElements GL_TRIANGLES vertexCount GL_UNSIGNED_SHORT nullPtr

getPlayerTransform :: HasSimulation s GameState => s -> Transform
getPlayerTransform = playerTransform . gPlayer . simState . getSimulation

uniformProjection :: Program -> IO ()
uniformProjection prog = do
  mbLoc <- getUniformLocation prog "proj"
  case mbLoc of
    Just loc -> useProgram prog >> uniform loc (persp !*! camera)
    _ -> pure ()

setModelTransform :: Program -> L.M44 Float -> IO ()
setModelTransform prog model = do
  Just loc <- getUniformLocation prog "model"
  useProgram prog >> uniform loc model

axisRenderer :: (HasResources s, HasSimulation s GameState,
                 MonadBaseControl IO m, MonadState s m) =>
                m ()
axisRenderer = do
  playerTrans <- gets getPlayerTransform
  axisProgram <- Res.loadProgram axisProgramDesc
  liftBase $ do
    useProgram axisProgram
    Just mvpLoc <- getUniformLocation axisProgram "mvp"
    let model = transform2M44 playerTrans
        mvp = persp L.!*! camera L.!*! model
    uniform mvpLoc mvp
    glDrawArrays GL_POINTS 0 1
