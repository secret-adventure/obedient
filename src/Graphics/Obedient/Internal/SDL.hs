module Graphics.Obedient.Internal.SDL where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans       (lift)
import           Control.Monad.Trans.Class ()
import           Control.Proxy
import           Control.Proxy.Concurrent
import           Control.Proxy.FRP         (Behavior (..), Event (..))
import qualified Control.Proxy.FRP         as FRP

import           Diagrams.Core.Points
import           Diagrams.TwoD.Types

import qualified Graphics.UI.SDL           as SDL
import qualified Graphics.UI.SDL.Events    as SDL

import           System.Exit

import           Graphics.Obedient.Internal

-- | Creates an SDL window with the given dimensions and returns a
--   stream of all of the events from the window.
initSDL :: Int -> Int -> Event (SDL.Event)
initSDL width height = Event $ \ () -> runIdentityP $ do
  lift $ do
    SDL.init [SDL.InitEverything]
    SDL.setVideoMode width height 32 []
  forever $ do
    event <- lift SDL.waitEvent
    respond event

  -- Prints every string in the event. Just for debugging for now.
runApp :: Event String -> IO ()
runApp (Event proxy) = runProxy $ proxy >-> foreverK printStrs
  where printStrs () = request () >>= lift . putStrLn

  -- TODO: Maybe allow specifying other options using a record, or
  -- something...
initializeWindow :: Int -> Int -> Inputs
initializeWindow width height =
  Inputs { quitEvent     = void $ FRP.filter (== SDL.Quit) events
         , leftClick     = FRP.mapMaybe (mouseDown SDL.ButtonLeft) events
         , rightClick    = FRP.mapMaybe (mouseDown SDL.ButtonRight) events
         , middleClick   = FRP.mapMaybe (mouseDown SDL.ButtonMiddle) events
         , mouseScroll   = FRP.mapMaybe mouseWheel events
         , mousePosition = FRP.behave mouseStart $ FRP.mapMaybe mouseMove events -- XXX: Get initial position properly.
         }
  where events = initSDL width height
        mouseStart = point (0, 0)

-- | Transforms the given integral coordinates into a two-dimensional
--   point.
point :: Integral a => (a, a) -> Point2D
point (x, y) = P $ r2 (fromIntegral x, fromIntegral y)

  -- TODO: Make this register for actual clicks rather than just mouse-down.
-- | Project mouseDown events for the given mouse button.
mouseDown :: SDL.MouseButton -> SDL.Event -> Maybe Point2D
mouseDown target (SDL.MouseButtonDown x y button)
  | target == button = Just $ point (x, y)
mouseDown _ _ = Nothing

-- | Project mouse scroll wheel events.
mouseWheel :: SDL.Event -> Maybe ScrollDirection
mouseWheel (SDL.MouseButtonDown _ _ SDL.ButtonWheelUp)   = Just ScrollUp
mouseWheel (SDL.MouseButtonDown _ _ SDL.ButtonWheelDown) = Just ScrollDown
mouseWheel _                                             = Nothing

-- | Project mouse move events to the absolute position of the mouse.
mouseMove :: SDL.Event -> Maybe Point2D
mouseMove (SDL.MouseMotion x y _ _) = Just $ point (x, y)
mouseMove _                         = Nothing