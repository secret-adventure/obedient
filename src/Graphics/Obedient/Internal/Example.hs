{-# LANGUAGE RecordWildCards #-}
module Graphics.Obedient.Internal.Example where

import qualified Graphics.Obedient.Internal     as OBD
import           Graphics.Obedient.Internal.SDL

  -- This imports all the mouse events/behaviors at the top level.
OBD.Inputs {..} = initializeWindow 400 400

main = runApp (fmap show leftClick)