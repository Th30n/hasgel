{-# LANGUAGE FlexibleContexts #-}

module Hasgel.Rendering (
  renderCameraOrientation, renderPlayer, renderPlayerShots, renderInvaders,
  axisRenderer
) where

import Control.Monad (when)
import System.Environment (getArgs)
import Foreign (nullPtr)

import Control.Monad.Base (MonadBase (..))
import Control.Monad.State (MonadState(..), gets)
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Graphics.GL.Core45
import Linear ((!*!))
import qualified Linear as L

import Hasgel.Simulation (Simulation(..), HasSimulation(..))
import Hasgel.Game (GameState(..), Player(..))
import Hasgel.Transform (Transform(..), transform2M44,
                         deg2Rad, defaultTransform)
import qualified Hasgel.GL as GL
import Hasgel.Mesh (Mesh, meshVertexCount)
import Hasgel.Resources (HasResources(..), Resources(..))
import qualified Hasgel.Resources as Res

mainProgramDesc :: Res.ProgramDesc
mainProgramDesc = [("shaders/basic.vert", GL.VertexShader),
                   ("shaders/basic.frag", GL.FragmentShader)]

normalsProgramDesc :: Res.ProgramDesc
normalsProgramDesc = [("shaders/basic.vert", GL.VertexShader),
                      ("shaders/normals.geom", GL.GeometryShader),
                      ("shaders/color.frag", GL.FragmentShader)]

axisProgramDesc :: Res.ProgramDesc
axisProgramDesc = [("shaders/axis.vert", GL.VertexShader),
                   ("shaders/axis.geom", GL.GeometryShader),
                   ("shaders/color.frag", GL.FragmentShader)]

persp :: L.M44 Float
persp = L.perspective fovy ar n f
        where fovy = deg2Rad 60
              ar = 800 / 600
              n = 0.1
              f = 100

ortho :: L.M44 Float
ortho = L.ortho (-10) 10 (-10) 10 (-10) 10

cameraTransform :: Transform
cameraTransform = defaultTransform { transformPosition = L.V3 0 (-10) (-21) }

camera :: L.M44 Float
camera = transform2M44 cameraTransform

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

renderMesh :: Mesh -> Transform -> GL.Program -> IO ()
renderMesh mesh transform prog = do
  GL.useProgram prog
  uniformProjection prog
  let vertexCount = meshVertexCount mesh
      model = transform2M44 transform
  setModelTransform prog model
  GL.drawElements GL_TRIANGLES vertexCount GL_UNSIGNED_SHORT nullPtr

renderCameraOrientation :: (HasResources s, MonadBaseControl IO m,
                            MonadState s m) => m ()
renderCameraOrientation = do
  vao <- gets $ resVao . getResources
  cameraVao <- gets $ resCameraVao . getResources
  let mvp = persp !*!
            transform2M44 cameraTransform { transformPosition = L.V3 0 0 (-1) }
  liftBase $ GL.bindVertexArray cameraVao
  liftBase $ GL.vertexAttrib3f (GL.Index 0) 0 0 0
  renderAxis 0.5 mvp
  liftBase $ GL.bindVertexArray vao

getPlayerTransform :: HasSimulation s GameState => s -> Transform
getPlayerTransform = playerTransform . gPlayer . simState . getSimulation

uniformProjection :: GL.Program -> IO ()
uniformProjection prog = do
  mbLoc <- GL.getUniformLocation prog "proj"
  case mbLoc of
    Just loc -> GL.useProgram prog >> GL.uniform loc (persp !*! camera)
    _ -> pure ()

setModelTransform :: GL.Program -> L.M44 Float -> IO ()
setModelTransform prog model = do
  Just loc <- GL.getUniformLocation prog "model"
  GL.useProgram prog >> GL.uniform loc model

axisRenderer :: (HasResources s, HasSimulation s GameState,
                 MonadBaseControl IO m, MonadState s m) =>
                m ()
axisRenderer = do
  playerTrans <- gets getPlayerTransform
  let model = transform2M44 playerTrans
      mvp = persp !*! camera !*! model
  renderAxis 2 mvp

renderAxis :: (HasResources s, MonadBaseControl IO m, MonadState s m) =>
              Float -> L.M44 Float -> m ()
renderAxis scale mvp = do
  axisProgram <- Res.loadProgram axisProgramDesc
  liftBase $ do
    GL.useProgram axisProgram
    Just scaleLoc <- GL.getUniformLocation axisProgram "scale"
    GL.uniform scaleLoc scale
    Just mvpLoc <- GL.getUniformLocation axisProgram "mvp"
    GL.uniform mvpLoc mvp
    glDrawArrays GL_POINTS 0 1
