module Graphics.Obedient.Internal.SDL where

import           Control.Applicative
import           Control.Monad
import           Control.Proxy.FRP         (Event (..))
import qualified Control.Proxy.FRP         as FRP

import           Diagrams.Core.Points
import           Diagrams.TwoD.Types

import qualified Graphics.UI.SDL           as SDL

import           Graphics.Obedient.Internal

-- | Creates an SDL window with the given dimensions and returns a
--   stream of all of the events from the window.
initSDL :: Int -> Int -> Event (SDL.Event)
initSDL width height = FRP.extractIO $ do
  SDL.init [SDL.InitEverything]
  SDL.setVideoMode width height 32 []
  return $ FRP.fromIO SDL.waitEvent

  -- TODO: Maybe allow specifying other options using a record, or
  -- something...
initializeWindow :: Int -> Int -> App
initializeWindow width height =
  App { quitEvent     = void $ FRP.filter (== SDL.Quit) events
      , leftClick     = FRP.mapMaybe (mouseDown SDL.ButtonLeft) events
      , rightClick    = FRP.mapMaybe (mouseDown SDL.ButtonRight) events
      , middleClick   = FRP.mapMaybe (mouseDown SDL.ButtonMiddle) events
      , mouseScroll   = FRP.mapMaybe mouseWheel events
      , mousePosition = FRP.behaveIO mouseStart $ FRP.mapMaybe mouseMove events
      }
  where events = initSDL width height
        mouseStart = (\ (x, y, _) -> point (x, y)) <$> SDL.getMouseState

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