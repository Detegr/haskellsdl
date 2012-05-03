import Graphics.UI.SDL.Image
import Graphics.UI.SDL
import Control.Monad

data Player = Player
            {
                x :: Int,
                y :: Int
            }
zeroPlr :: Player
zeroPlr = Player 0 0

data GameState = GameState
               {
                   player     :: Player,
                   keys       :: KeyState,
                   notRunning :: Bool
               }
mkSt :: Player -> KeyState -> GameState
mkSt player keystate = GameState player keystate False

data KeyState = KeyState
              {
                   keyUp    :: Bool,
                   keyDown  :: Bool,
                   keyLeft  :: Bool,
                   keyRight :: Bool
              }
zeroKeys :: KeyState
zeroKeys = KeyState False False False False

data GameData = GameData
              {
                  screen :: Surface,
                  sprite :: Surface
              }
zeroState :: GameState
zeroState = GameState zeroPlr zeroKeys False

loadImage :: String -> IO Surface
loadImage f = load f >>= displayFormat

applySurface :: Int -> Int -> Surface -> Surface -> IO Bool
applySurface x y src dst = blitSurface src Nothing dst offset
   where offset = Just (Rect x y 0 0)

loop :: GameData -> GameState -> IO ()
loop g s = do state <- pollEvents s
              let (Player x y) = player s
              applySurface x y (sprite g) (screen g)
              Graphics.UI.SDL.flip (screen g)
              unless (notRunning state) (loop g state)

pollEvents :: GameState -> IO GameState
pollEvents g = pollEvent >>= \e -> 
               case e of Quit      -> return $ (GameState (player g) zeroKeys True)
                         KeyDown k -> return $ setKey k True g
                         KeyUp   k -> return $ setKey k False g
                         _         -> return $ tick g

setKey :: Keysym -> Bool -> GameState -> GameState
setKey (Keysym k _ _) b g = case k of SDLK_UP    -> mkSt (player g) (KeyState b d l r)
                                      SDLK_DOWN  -> mkSt (player g) (KeyState u b l r)
                                      SDLK_LEFT  -> mkSt (player g) (KeyState u d b r)
                                      SDLK_RIGHT -> mkSt (player g) (KeyState u d l b)
                                      _          -> g
                          where (KeyState u d l r) = keys g

movePlayer :: KeyState -> Player -> Player
movePlayer (KeyState u d l r) (Player x y)
    | u && l = Player (x-1) (y-1)
    | u && r = Player (x+1) (y-1)
    | u      = Player x (y-1)
    | d && l = Player (x-1) (y+1)
    | d && r = Player (x+1) (y+1)
    | d      = Player x (y+1)
    | l      = Player (x-1) y
    | r      = Player (x+1) y
    | otherwise = Player x y

tick :: GameState -> GameState
tick g = GameState (movePlayer (keys g) (player g)) (keys g) False

initData :: IO GameData
initData = do screen <- setVideoMode 800 600 32 [SWSurface]
              setCaption "test" []
              image <- loadImage "img/dude.png"
              return $ GameData screen image

main = withInit [InitEverything] $ do
       gdata <- initData
       loop gdata zeroState
