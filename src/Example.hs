{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Applicative

import           Diagrams.Core.Points
import           Diagrams.TwoD.Types

import qualified Graphics.Obedient.Internal     as OBD
import           Graphics.Obedient.Internal.SDL

  -- This imports all the mouse events/behaviors at the top level.
OBD.App {..} = initializeWindow 400 400

main = render $ toRect <$> mousePosition
  where toRect = rect . unr2 . (\ (P x) -> x)
        rect (x, y) = OBD.Rectangle (truncate x) (truncate y) 25 25
