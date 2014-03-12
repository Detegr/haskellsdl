{-# LANGUAGE TemplateHaskell, DoRec #-}

import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Lens
import Data.Default
import Graphics.UI.SDL
import Graphics.UI.SDL.Image
import Control.Monad
import Control.Applicative ((<$>),(<*>))
import FRP.Sodium
import Control.Concurrent
import Debug.Trace
import Data.IORef

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
                           , _game     :: GameData
                           , _keyevent :: FRP.Sodium.Event SDLKey}
makeLenses ''GameState

loadImage :: String -> IO Surface
loadImage f = load f >>= displayFormat

applySurface :: Int -> Int -> Surface -> Surface -> IO Bool
applySurface x y src dst = blitSurface src Nothing dst offset
   where offset = Just (Rect x y 0 0)

loop :: StateT GameState IO ()
loop = get >>= \st -> do
  liftIO $ putStrLn "jee"
  rec
    liftIO $ putStrLn "foo?"
    keyb <- liftIO $ sync $ hold SDLK_UNKNOWN (merge (st ^. keyevent) keyPresses)
    let keyPresses = snapshot (\e b -> e) (st ^. keyevent) keyb
    liftIO $ putStrLn "foo!"
  --pollEvents
  movePlayerM
  liftIO $ putStrLn "bar!"
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

initState :: FRP.Sodium.Event SDLKey -> IO GameState
initState e = do
  setCaption "test" []
  gdata <- GameData <$>
             setVideoMode 800 600 32 [SWSurface] <*>
             loadImage "img/dude.png"
  return $ GameState def def True gdata e

handleEvent :: Graphics.UI.SDL.Event -> Player -> Player
handleEvent e b =
  case e of
    KeyDown k -> plrX +~ 1 $ b

playerLogic :: FRP.Sodium.Event Graphics.UI.SDL.Event -> Reactive (Behaviour Player)
playerLogic keyEvent = do
  rec
    keybeh <- hold NoEvent keyEvent
    let keybehs = snapshot handleEvent keyEvent plrbeh
    plrbeh <- hold (def :: Player) keybehs
  return plrbeh

main :: IO()
main = withInit [InitEverything] $ do
  (keyEvent, pushKey) <- sync newEvent
  _ <- forkIO $ do
    let doPoll = pollEvent >>= \e ->
                 case e of
                   Quit -> sync (pushKey e) >> doPoll
                   KeyDown _ -> sync (pushKey e) >> doPoll
                   _ -> doPoll
    doPoll

  setCaption "test" []
  screen <- setVideoMode 800 600 32 [SWSurface]
  dude <- loadImage "img/dude.png"
  plr <- newIORef (def :: Player)
  _ <- forkIO $
    let doDraw = do
        plr' <- readIORef plr
        _ <- applySurface (plr' ^. plrX) (plr' ^. plrY) dude screen
        Graphics.UI.SDL.flip screen
        doDraw
    in doDraw

  unlisten <- sync $ do
    player <- playerLogic keyEvent
    listen (value player) (writeIORef plr)

  let loop = loop
  return $! loop
  return ()
  --initState keyEvent >>= void . evalStateT loop
  --unlisten <- sync $ do
    --key <- hold (def :: KeyState) keyEvent
    --initState >>= void . evalStateT loop
