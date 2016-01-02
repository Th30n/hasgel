{-# LANGUAGE FlexibleContexts #-}

module Hasgel.Rendering (
  Camera(..), defaultCamera, viewForward, viewBack, viewRight, viewLeft,
  viewUp, viewDown,
  renderCameraOrientation, renderPlayer, renderPlayerShots, renderInvaders,
  axisRenderer, renderGamma, defaultGamma
) where

import Control.Monad (when)
import Control.Arrow ((&&&))
import Control.Lens ((.~), (^.))
import Control.Monad.Base (MonadBase (..))
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState(..), gets)
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Graphics.GL.Core45
import Linear ((!*!))
import qualified Linear as L

import Hasgel.Args (Args (..))
import Hasgel.Simulation (Simulation(..), HasSimulation(..))
import Hasgel.Game (GameState(..), Player(..))
import Hasgel.Transform
import Hasgel.Drawable
import qualified Hasgel.GL as GL
import Hasgel.Resources (HasResources(..), Resources(..))
import qualified Hasgel.Resources as Res

data Camera = Camera
  { cameraTransform :: Transform
  , cameraProjection :: L.M44 Float
  } deriving (Show)

defaultGamma :: Float
defaultGamma = 2.2

mainProgramDesc :: Res.ProgramDesc
mainProgramDesc = [("shaders/basic.vert", GL.VertexShader),
                   ("shaders/basic.frag", GL.FragmentShader)]

gammaProgramDesc :: Res.ProgramDesc
gammaProgramDesc = [("shaders/pass.vert", GL.VertexShader),
                    ("shaders/gamma.frag", GL.FragmentShader)]

gouraudProgramDesc :: Res.ProgramDesc
gouraudProgramDesc = [("shaders/gouraud.vert", GL.VertexShader),
                      ("shaders/gouraud.frag", GL.FragmentShader)]

normalsProgramDesc :: Res.ProgramDesc
normalsProgramDesc = [("shaders/basic.vert", GL.VertexShader),
                      ("shaders/normals.geom", GL.GeometryShader),
                      ("shaders/color.frag", GL.FragmentShader)]

axisProgramDesc :: Res.ProgramDesc
axisProgramDesc = [("shaders/axis.vert", GL.VertexShader),
                   ("shaders/axis.geom", GL.GeometryShader),
                   ("shaders/color.frag", GL.FragmentShader)]

persp :: L.M44 Float
persp = persp''
        where fovy = deg2Rad 60
              ar = 800 / 600
              n = 0.1
              f = 100
              -- Workaround for left out multiplication by 2
              persp' = L.perspective fovy ar n f
              w = persp' ^. L._z.L._w
              persp'' = L._z.L._w .~ (2*w) $ persp'

ortho :: L.M44 Float
ortho = L.ortho (-2) 2 (-2) 2 (-2) 2

defaultCamera :: Camera
defaultCamera = Camera {
  cameraTransform = defaultTransform { transformPosition = L.V3 0 10 20 },
  cameraProjection = persp }

-- | Return the view rotation. This is the inverse of camera rotation.
viewRotation :: Camera -> L.Quaternion Float
viewRotation = L.conjugate . transformRotation . cameraTransform

-- | Return the forward vector of the view orientation.
-- This is the back vector of the camera orientation.
viewForward :: Camera -> L.V3 Float
viewForward = transformBack . cameraTransform

-- | Return the back vector of the view orientation.
-- This is the forward vector of the camera orientation.
viewBack :: Camera -> L.V3 Float
viewBack = transformForward . cameraTransform

viewLeft :: Camera -> L.V3 Float
viewLeft = transformLeft . cameraTransform

viewRight :: Camera -> L.V3 Float
viewRight = transformRight . cameraTransform

viewDown :: Camera -> L.V3 Float
viewDown = transformDown . cameraTransform

viewUp :: Camera -> L.V3 Float
viewUp = transformUp . cameraTransform

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
                      MonadBaseControl IO m, MonadState s m,
                      MonadReader Args m) =>
                     Camera -> m ()
renderPlayerShots camera = do
  playerShots <- gets $ gPlayerShots . simState . getSimulation
  mapM_ (cubeRenderer camera) playerShots

renderInvaders :: (HasResources s, HasSimulation s GameState,
                   MonadBaseControl IO m, MonadState s m, MonadReader Args m) =>
                  Camera -> m ()
renderInvaders camera = do
  invaders <- gets $ gInvaders . simState . getSimulation
  mapM_ (cubeRenderer camera) invaders

renderPlayer :: (HasResources s, HasSimulation s GameState,
                 MonadBaseControl IO m, MonadState s m, MonadReader Args m) =>
                Camera -> m ()
renderPlayer camera = cubeRenderer camera =<< gets getPlayerTransform

cubeRenderer :: (HasResources s, HasSimulation s GameState,
                 MonadBaseControl IO m, MonadState s m, MonadReader Args m) =>
                Camera -> Transform -> m ()
cubeRenderer camera transform = do
  mainProg <- Res.loadProgram gouraudProgramDesc
  normalsProg <- Res.loadProgram normalsProgramDesc
  Just ship <- Res.getDrawable "player-ship"
  let mvp = cameraViewProjection camera !*! transform2M44 transform
      -- Normal transform assumes uniform scaling.
      normalModel = transform2M44 transform ^. L._m33
      mv = cameraView camera !*! transform2M44 transform
  liftBase $ do
    GL.useProgram mainProg $ do
      GL.uniformByName "mvp" mvp
      GL.uniformByName "normal_model" normalModel
      GL.uniformByName "mv" mv
      GL.uniformByName "mat.spec" $ (L.V3 0.8 0.8 0.8 :: L.V3 Float)
      GL.uniformByName "mat.shine" (25 :: Float)
    draw ship
  renderNormals <- asks argsNormals
  liftBase . when renderNormals $ do
    GL.useProgram normalsProg $
      GL.uniformByName "mvp" mvp
    draw ship

renderCameraOrientation :: (HasResources s, MonadBaseControl IO m,
                            MonadState s m) => Camera -> m ()
renderCameraOrientation camera = do
  let rot = L.fromQuaternion $ viewRotation camera
      mvp = ortho !*! L.m33_to_m44 rot
  renderAxis 1 mvp

getPlayerTransform :: HasSimulation s GameState => s -> Transform
getPlayerTransform = playerTransform . gPlayer . simState . getSimulation

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
  axisVao <- gets $ resAxisVao . getResources
  liftBase $ do
    GL.bindVertexArray axisVao
    GL.vertexAttrib3f (GL.Index 0) 0 0 0
    GL.useProgram axisProgram $ do
      GL.uniformByName "scale" scale
      GL.uniformByName "mvp" mvp
    glDrawArrays GL_POINTS 0 1

-- | Post processing effect. Renders a fullscreen quad from given texture and
-- applies gamma correction.
renderGamma :: (HasResources s, MonadBaseControl IO m, MonadState s m) =>
               Float -> GL.Texture -> m ()
renderGamma gamma texture = do
  prog <- Res.loadProgram gammaProgramDesc
  Just plane <- Res.getDrawable "plane"
  liftBase $ do
    glActiveTexture GL_TEXTURE0
    glBindTexture GL_TEXTURE_2D $ GL.object texture
    let model = rotateLocal defaultTransform $ L.V3 90 0 0
    GL.useProgram prog $ do
      GL.uniformByName "mvp" $ transform2M44 model
      GL.uniformByName "gamma" gamma
    draw plane
