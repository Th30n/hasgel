{-# LANGUAGE FlexibleContexts #-}

module Hasgel.Rendering (
  Camera(..), defaultCamera, viewForward,
  renderCameraOrientation, renderPlayer, renderPlayerShots, renderInvaders,
  axisRenderer
) where

import Control.Monad (when)
import Control.Arrow ((&&&))
import System.Environment (getArgs)
import Foreign (nullPtr)

import Control.Lens ((.~))
import Control.Monad.Base (MonadBase (..))
import Control.Monad.State (MonadState(..), gets)
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Graphics.GL.Core45
import Linear ((!*!))
import qualified Linear as L

import Hasgel.Simulation (Simulation(..), HasSimulation(..))
import Hasgel.Game (GameState(..), Player(..))
import Hasgel.Transform (Transform(..), transform2M44, transformBack,
                         deg2Rad, defaultTransform)
import qualified Hasgel.GL as GL
import Hasgel.Mesh (Mesh, meshVertexCount)
import Hasgel.Resources (HasResources(..), Resources(..))
import qualified Hasgel.Resources as Res

data Camera = Camera
  { cameraTransform :: Transform
  , cameraProjection :: L.M44 Float
  } deriving (Show)

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
ortho = L.ortho (-2) 2 (-2) 2 (-2) 2

defaultCamera :: Camera
defaultCamera = Camera {
  cameraTransform = defaultTransform { transformPosition = L.V3 0 10 21 },
  cameraProjection = persp }

-- | Return the view rotation. This is the inverse of camera rotation.
viewRotation :: Camera -> L.Quaternion Float
viewRotation = L.conjugate . transformRotation . cameraTransform

-- | Return the forward vector of the view orientation.
-- This is the back vector of the camera orientation.
viewForward :: Camera -> L.V3 Float
viewForward = transformBack . cameraTransform

-- | Return the view matrix for the given camera.
cameraView :: Camera -> L.M44 Float
cameraView camera =
  let transform = cameraTransform camera
      pos = -transformPosition transform
      trans = L.translation .~ pos $ L.identity
      rot = L.fromQuaternion $ viewRotation camera
  in L.m33_to_m44 rot !*! trans -- Inverse of camera transform

-- | Return the view projection matrix for the given camera.
cameraViewProjection :: Camera -> L.M44 Float
cameraViewProjection = uncurry (!*!) . (cameraProjection &&& cameraView)

renderPlayerShots :: (HasResources s, HasSimulation s GameState,
                      MonadBaseControl IO m, MonadState s m) =>
                     Camera -> m ()
renderPlayerShots camera = do
  playerShots <- gets $ gPlayerShots . simState . getSimulation
  mapM_ (cubeRenderer camera) playerShots

renderInvaders :: (HasResources s, HasSimulation s GameState,
                   MonadBaseControl IO m, MonadState s m) =>
                  Camera -> m ()
renderInvaders camera = do
  invaders <- gets $ gInvaders . simState . getSimulation
  mapM_ (cubeRenderer camera) invaders

renderPlayer :: (HasResources s, HasSimulation s GameState,
                 MonadBaseControl IO m, MonadState s m) =>
                Camera -> m ()
renderPlayer camera = cubeRenderer camera =<< gets getPlayerTransform

cubeRenderer :: (HasResources s, HasSimulation s GameState,
                 MonadBaseControl IO m, MonadState s m) =>
                Camera -> Transform -> m ()
cubeRenderer camera transform = do
  mainProg <- Res.loadProgram mainProgramDesc
  normalsProg <- Res.loadProgram normalsProgramDesc
  res <- gets Res.getResources
  let renderCube = renderMesh camera (resMesh res) transform
  liftBase $ do
    renderCube mainProg
    args <- getArgs
    when ("-normals" `elem` args) $ renderCube normalsProg

renderMesh :: Camera -> Mesh -> Transform -> GL.Program -> IO ()
renderMesh camera mesh transform prog = do
  GL.useProgram prog
  uniformProjection camera prog
  let vertexCount = meshVertexCount mesh
      model = transform2M44 transform
  setModelTransform prog model
  GL.drawElements GL_TRIANGLES vertexCount GL_UNSIGNED_SHORT nullPtr

renderCameraOrientation :: (HasResources s, MonadBaseControl IO m,
                            MonadState s m) => Camera -> m ()
renderCameraOrientation camera = do
  let rot = L.fromQuaternion $ viewRotation camera
      mvp = ortho !*! L.m33_to_m44 rot
  renderAxis 1 mvp

getPlayerTransform :: HasSimulation s GameState => s -> Transform
getPlayerTransform = playerTransform . gPlayer . simState . getSimulation

uniformProjection :: Camera -> GL.Program -> IO ()
uniformProjection camera prog = do
  mbLoc <- GL.getUniformLocation prog "proj"
  let vp = cameraViewProjection camera
  case mbLoc of
    Just loc -> GL.useProgram prog >> GL.uniform loc vp
    _ -> pure ()

setModelTransform :: GL.Program -> L.M44 Float -> IO ()
setModelTransform prog model = do
  Just loc <- GL.getUniformLocation prog "model"
  GL.useProgram prog >> GL.uniform loc model

axisRenderer :: (HasResources s, HasSimulation s GameState,
                 MonadBaseControl IO m, MonadState s m) =>
                Camera -> m ()
axisRenderer camera = do
  playerTrans <- gets getPlayerTransform
  let model = transform2M44 playerTrans
      mvp = cameraViewProjection camera !*! model
  renderAxis 2 mvp

renderAxis :: (HasResources s, MonadBaseControl IO m, MonadState s m) =>
              Float -> L.M44 Float -> m ()
renderAxis scale mvp = do
  axisProgram <- Res.loadProgram axisProgramDesc
  vao <- gets $ resVao . getResources
  axisVao <- gets $ resAxisVao . getResources
  liftBase $ do
    GL.bindVertexArray axisVao
    GL.vertexAttrib3f (GL.Index 0) 0 0 0
    GL.useProgram axisProgram
    Just scaleLoc <- GL.getUniformLocation axisProgram "scale"
    GL.uniform scaleLoc scale
    Just mvpLoc <- GL.getUniformLocation axisProgram "mvp"
    GL.uniform mvpLoc mvp
    glDrawArrays GL_POINTS 0 1
    GL.bindVertexArray vao
