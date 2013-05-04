{-# LANGUAGE RecordWildCards #-}
module Graphics.Obedient.Internal.SDL where

import           Control.Applicative
import           Control.Concurrent         (threadDelay)
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Proxy.FRP          (Behavior (..))
import qualified Control.Proxy.FRP          as FRP

import           Diagrams.Core.Points
import           Diagrams.TwoD.Types

import qualified Graphics.UI.SDL            as SDL

import           Graphics.Obedient.Internal

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
      , render        = void . render
      }
  where initial = do SDL.init [SDL.InitEverything]
                     surface <- SDL.setVideoMode width height 32 []
                     return (surface, FRP.fromIO SDL.waitEvent)
        events = FRP.extractIO $ snd <$> initial

        mouseStart = (\ (x, y, _) -> point (x, y)) <$> SDL.getMouseState
        
        render (Behavior rectSTM) = forever $ do
          threadDelay 3000
          rect <- rectSTM >>= atomically
          surface <- fst <$> initial
          SDL.fillRect surface (Just $ toRect rect) (SDL.Pixel 0x3366FF)
          SDL.flip surface
        toRect (Rectangle {..}) = SDL.Rect x y width height
          
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
