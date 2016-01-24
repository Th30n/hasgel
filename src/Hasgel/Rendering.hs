{-# LANGUAGE FlexibleContexts #-}

module Hasgel.Rendering (
  Camera(..), defaultCamera, viewForward, viewBack, viewRight, viewLeft,
  viewUp, viewDown,
  renderCameraOrientation, renderPlayer, renderShots, renderInvaders,
  renderString, axisRenderer, renderGamma, defaultGamma
) where

import Control.Lens ((.~), (^.))
import Control.Monad (forM_, when)
import Control.Monad.Base (MonadBase (..))
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState(..), gets)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Data.Char (ord)
import Data.Int (Int32)
import Data.Maybe (fromMaybe)

import Graphics.GL.Core45
import Linear ((!*!))
import qualified Linear as L

import Hasgel.Args (Args (..))
import Hasgel.Simulation
import Hasgel.Game (GameState(..), Player(..), Invader(..))
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

explodeProgramDesc :: Res.ProgramDesc
explodeProgramDesc = ("shaders/explode.geom", GL.GeometryShader) :
                     gouraudProgramDesc

spriteProgramDesc :: Res.ProgramDesc
spriteProgramDesc = [("shaders/billboard.vert", GL.VertexShader),
                     ("shaders/billboard.geom", GL.GeometryShader),
                     ("shaders/billboard.frag", GL.FragmentShader)]

textProgramDesc :: Res.ProgramDesc
textProgramDesc = [("shaders/billboard.vert", GL.VertexShader),
                   ("shaders/billboard.geom", GL.GeometryShader),
                   ("shaders/text.frag", GL.FragmentShader)]

normalsProgramDesc :: Res.ProgramDesc
normalsProgramDesc = [("shaders/basic.vert", GL.VertexShader),
                      ("shaders/normals.geom", GL.GeometryShader),
                      ("shaders/color.frag", GL.FragmentShader)]

axisProgramDesc :: Res.ProgramDesc
axisProgramDesc = [("shaders/axis.vert", GL.VertexShader),
                   ("shaders/axis.geom", GL.GeometryShader),
                   ("shaders/color.frag", GL.FragmentShader)]

persp :: L.V2 Float -> L.M44 Float
persp (L.V2 width height) = persp''
        where fovy = deg2Rad 60
              ar = width / height
              n = 0.1
              f = 100
              -- Workaround for left out multiplication by 2
              persp' = L.perspective fovy ar n f
              w = persp' ^. L._z.L._w
              persp'' = L._z.L._w .~ (2*w) $ persp'

ortho :: L.M44 Float
ortho = L.ortho (-2) 2 (-2) 2 (-2) 2

defaultCamera :: L.V2 Float -> Camera
defaultCamera dim = Camera {
  cameraTransform = defaultTransform { transformPosition = L.V3 0 10 20 },
  cameraProjection = persp dim }

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
cameraViewProjection = (!*!) <$> cameraProjection <*> cameraView

renderShots :: (HasResources s, HasSimulation s GameState, MonadState s m,
                MonadBaseControl IO m) => Camera -> m ()
renderShots camera = do
  shots <- gets $ gShots . simState . getSimulation
  forM_ shots $ \transform -> do
    spriteProgram <- Res.loadProgram spriteProgramDesc
    Just point <- Res.getDrawable "point"
    texture <- gets $ resLaserTex . getResources
    liftBase $ do
      glActiveTexture GL_TEXTURE0
      glBindTexture GL_TEXTURE_2D $ GL.object texture
      let mv = cameraView camera !*! transform2M44 transform
      GL.useProgram spriteProgram $ do
        GL.uniformByName "mv" mv
        GL.uniformByName "proj" $ cameraProjection camera
        GL.uniformByName "size" $ transformScale transform ^. L._x
      draw point

renderInvaders :: (HasResources s, HasSimulation s GameState,
                   MonadBaseControl IO m, MonadState s m, MonadReader Args m) =>
                  Camera -> m ()
renderInvaders camera = do
  sim <- gets getSimulation
  let invaders = gInvaders . simState $ sim
      exploding = gExploded . simState $ sim
  mapM_ (shipRenderer camera . iTransform) invaders
  forM_ exploding $ \invader -> do
    let time = timeCurrent . simTime $ sim
        dt = fromMaybe 0 $ (time -) <$> iExplodeTime invader
        transform = iTransform invader
    explodingShipRenderer camera transform dt

renderPlayer :: (HasResources s, HasSimulation s GameState,
                 MonadBaseControl IO m, MonadState s m, MonadReader Args m) =>
                Camera -> m ()
renderPlayer camera = do
  sim <- gets getSimulation
  let player = gPlayer $ simState sim
      transform = pTransform player
  case pExplodeTime player of
    Nothing -> shipRenderer camera transform
    Just explodeTime -> do
      let time = timeCurrent . simTime $ sim
      explodingShipRenderer camera transform (time - explodeTime)

shipRenderer :: (HasResources s, MonadBaseControl IO m, MonadState s m,
                 MonadReader Args m) => Camera -> Transform -> m ()
shipRenderer camera transform = do
  mainProg <- Res.loadProgram gouraudProgramDesc
  renderShip mainProg camera transform
  normalsProg <- Res.loadProgram normalsProgramDesc
  Just ship <- Res.getDrawable "player-ship"
  let mvp = cameraViewProjection camera !*! transform2M44 transform
  renderNormals <- asks argsNormals
  liftBase . when renderNormals $ do
    GL.useProgram normalsProg $
      GL.uniformByName "mvp" mvp
    draw ship

explodingShipRenderer :: (HasResources s, MonadBaseControl IO m, MonadState s m) =>
                         Camera -> Transform -> Milliseconds -> m ()
explodingShipRenderer camera transform explodeTime = do
    prog <- Res.loadProgram explodeProgramDesc
    let exFactor :: Float
        exFactor = 8 * millis2Sec explodeTime
    liftBase . GL.useProgram prog $
      GL.uniformByName "explode_factor" exFactor
    renderShip prog camera transform

renderShip :: (HasResources s, MonadBase IO m, MonadState s m) =>
              GL.Program -> Camera -> Transform -> m ()
renderShip program camera transform = do
  Just ship <- Res.getDrawable "player-ship"
  tex <- gets $ resTex . getResources
  let mvp = cameraViewProjection camera !*! transform2M44 transform
      -- Normal transform assumes uniform scaling.
      normalModel = transform2M44 transform ^. L._m33
      mv = cameraView camera !*! transform2M44 transform
  liftBase $ do
    glActiveTexture GL_TEXTURE0
    glBindTexture GL_TEXTURE_2D $ GL.object tex
    GL.useProgram program $ do
      GL.uniformByName "mvp" mvp
      GL.uniformByName "normal_model" normalModel
      GL.uniformByName "mv" mv
      GL.uniformByName "mat.spec" (L.V3 0.8 0.8 0.8 :: L.V3 Float)
      GL.uniformByName "mat.shine" (25 :: Float)
    draw ship

renderCameraOrientation :: (HasResources s, MonadBaseControl IO m,
                            MonadState s m) => Camera -> m ()
renderCameraOrientation camera = do
  let rot = L.fromQuaternion $ viewRotation camera
      mvp = ortho !*! L.m33_to_m44 rot
  renderAxis 1 mvp

axisRenderer :: (HasResources s, HasSimulation s GameState,
                 MonadBaseControl IO m, MonadState s m) =>
                Camera -> m ()
axisRenderer camera = do
  playerTrans <- gets $ pTransform . gPlayer . simState . getSimulation
  let model = transform2M44 playerTrans
      mvp = cameraViewProjection camera !*! model
  renderAxis 2 mvp

renderAxis :: (HasResources s, MonadBaseControl IO m, MonadState s m) =>
              Float -> L.M44 Float -> m ()
renderAxis scale mvp = do
  axisProgram <- Res.loadProgram axisProgramDesc
  Just point <- Res.getDrawable "point"
  liftBase $ do
    GL.useProgram axisProgram $ do
      GL.uniformByName "scale" scale
      GL.uniformByName "mvp" mvp
    draw point

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

-- | Construct an orthographic projection that corresponds with
-- screen resolution.
screenOrtho :: Float -> Float -> L.M44 Float
screenOrtho width height = L.ortho 0 width 0 height (-1) 1

renderString :: (HasResources s, MonadState s m, MonadBaseControl IO m) =>
                L.V2 Float -> Float -> Float -> String -> m ()
renderString (L.V2 scrW scrH) x y text = do
  prog <- Res.loadProgram textProgramDesc
  Just plane <- Res.getDrawable "point"
  texture <- gets $ resFontTex . getResources
  liftBase $ do
    glActiveTexture GL_TEXTURE0
    glBindTexture GL_TEXTURE_2D $ GL.object texture
    let proj = screenOrtho scrW scrH
        cellWidth = 16 :: Int32
        cellHeight = 16 :: Int32
        charPos = charPosForFont 256 cellWidth cellHeight
    GL.useProgram prog $ do
      GL.uniformByName "proj" proj
      GL.uniformByName "size" (0.5 * fromIntegral cellWidth :: Float)
      GL.uniformByName "cell" $ L.V2 cellWidth cellHeight
      forM_ (zip [0..] text) $ \(i, c) -> do
        GL.uniformByName "char_pos" $ charPos c
        let currX = x + 0.5 * i * fromIntegral cellWidth
            model = translate defaultTransform $ L.V3 currX y 0
        GL.uniformByName "mv" $ transform2M44 model
        lift $ draw plane

charPosForFont :: Int32 -> Int32 -> Int32 -> Char -> L.V2 Int32
charPosForFont width cellWidth cellHeight char
  | ord char < 0 || ord char > 255 = error "Only ASCII supported."
  | otherwise = L.V2 (x * cellWidth) (y * cellHeight)
  where charsPerLine = width `div` cellWidth
        charNum = fromIntegral $ ord char - 32
        x = charNum `mod` charsPerLine
        y = charNum `div` charsPerLine
