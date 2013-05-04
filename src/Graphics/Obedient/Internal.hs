{-# LANGUAGE EmptyDataDecls #-}
module Graphics.Obedient.Internal where

import           Control.Proxy.FRP    (Behavior (..), Event (..))

import           Diagrams.Core.Points
import           Diagrams.TwoD.Types

type Point2D = Point R2

-- A trivial geometric type standing in for Diagrams or whatever.
data Rectangle = Rectangle { x, y, width, height :: Int } 

data ScrollDirection
   = ScrollUp
   | ScrollDown deriving (Show, Eq, Enum)

-- This could be used nicely with record wildcard patterns:
-- App { .. } = initialize foo bar
data App = App { quitEvent     :: Event ()
               , leftClick     :: Event Point2D
               , rightClick    :: Event Point2D
               , middleClick   :: Event Point2D
               , mouseScroll   :: Event ScrollDirection
               , mousePosition :: Behavior Point2D
               , render        :: Behavior Rectangle -> IO ()
               }
