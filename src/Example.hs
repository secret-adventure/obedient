{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Applicative
import qualified Control.Proxy.FRP              as FRP

import qualified Graphics.Obedient.Internal     as OBD
import           Graphics.Obedient.Internal.SDL

  -- This imports all the mouse events/behaviors at the top level.
OBD.App {..} = initializeWindow 400 400

main = FRP.runIO $ print <$> leftClick
