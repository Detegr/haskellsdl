{-# LANGUAGE TemplateHaskell #-}

import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Lens
import Data.Default
import Graphics.UI.SDL
import Graphics.UI.SDL.Image
import Control.Monad
import Control.Applicative ((<$>),(<*>))

data Player = Player { _plrX :: Int
                     , _plrY :: Int }
makeLenses ''Player

instance Default Player where
  def = Player 0 0

data KeyState = KeyState { _keyUp        :: Bool
                         , _keyDown      :: Bool
                         , _keyLeft      :: Bool
                         , _keyRight     :: Bool
                         , _keyUnhandled :: Bool}
makeLenses ''KeyState
instance Default KeyState where
  def = KeyState False False False False False

data GameData = GameData { _screen :: Surface
                         , _sprite :: Surface }
makeLenses ''GameData

data GameState = GameState { _player   :: Player
                           , _keys     :: KeyState
                           , _running  :: Bool
                           , _game     :: GameData}
makeLenses ''GameState

loadImage :: String -> IO Surface
loadImage f = load f >>= displayFormat

applySurface :: Int -> Int -> Surface -> Surface -> IO Bool
applySurface x y src dst = blitSurface src Nothing dst offset
   where offset = Just (Rect x y 0 0)

loop :: StateT GameState IO ()
loop = get >>= \st -> do
  pollEvents
  movePlayerM
  liftIO $ do
    _ <- applySurface (st ^. player.plrX)
                      (st ^. player.plrY)
                      (st ^. game.sprite)
                      (st ^. game.screen)
    Graphics.UI.SDL.flip (st ^. game.screen)
  when (st ^. running) loop


pollEvents :: StateT GameState IO ()
pollEvents = liftIO pollEvent >>= \e -> get >>= \st ->
             case e of
                Quit      -> put $ running  .~ False $ st
                KeyDown k -> put $ getKey k .~ True  $ st
                KeyUp   k -> put $ getKey k .~ False $ st
                _         -> return ()

getKey :: Functor f => Keysym -> (Bool -> f Bool) -> GameState -> f GameState
getKey (Keysym k _ _) =
  case k of
    SDLK_UP    -> keys . keyUp
    SDLK_DOWN  -> keys . keyDown
    SDLK_LEFT  -> keys . keyLeft
    SDLK_RIGHT -> keys . keyRight
    _          -> keys . keyUnhandled

fromBool :: Bool -> Int
fromBool b = if b then 1 else 0

movePlayer :: GameState -> GameState
movePlayer g = player . plrX +~ fromBool (ks ^. keyRight) $
               player . plrX -~ fromBool (ks ^. keyLeft) $
               player . plrY +~ fromBool (ks ^. keyDown) $
               player . plrY -~ fromBool (ks ^. keyUp) $ g
  where ks = g ^. keys

movePlayerM :: StateT GameState IO ()
movePlayerM = get >>= put . movePlayer

initState :: IO GameState
initState = do
  setCaption "test" []
  gdata <- GameData <$>
             setVideoMode 800 600 32 [SWSurface] <*>
             loadImage "img/dude.png"
  return $ GameState def def True gdata

main :: IO()
main = withInit [InitEverything] $ initState >>= void . evalStateT loop
