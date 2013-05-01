{-# LANGUAGE EmptyDataDecls #-}
module Graphics.Obedient.Internal where

import           Control.Proxy.FRP    (Behavior (..), Event (..))
import qualified Control.Proxy.FRP    as FRP

import           Diagrams.Core.Points
import           Diagrams.TwoD.Types

type Point2D = Point R2

data ScrollDirection
   = ScrollUp
   | ScrollDown deriving (Show, Eq, Enum)

-- This could be used nicely with record wildcard patterns:
-- Input { .. } = initialize foo bar
data Inputs = Inputs { quitEvent     :: Event ()
                     , leftClick     :: Event Point2D
                     , rightClick    :: Event Point2D
                     , middleClick   :: Event Point2D
                     , mouseScroll   :: Event ScrollDirection
                     , mousePosition :: Behavior Point2D }
