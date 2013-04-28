module Graphics.Obedient.Internal.SDL where

import           Control.Monad.Trans       (lift)
import           Control.Monad.Trans.Class ()
import           Control.Proxy
import           Control.Proxy.Concurrent
import           Control.Proxy.FRP

import           Diagrams.Core.Points
import           Diagrams.TwoD.Types

import qualified Graphics.UI.SDL           as SDL
import qualified Graphics.UI.SDL.Events    as SDL

initSDL :: Int -> Int -> Event (SDL.Event)
initSDL width height = Event $ \ () -> runIdentityP $ do
  lift $ do
    SDL.init [SDL.InitEverything]
    SDL.setVideoMode width height 32 []
  forever $ do event <- lift SDL.waitEvent
               lift $ print event
               respond event
