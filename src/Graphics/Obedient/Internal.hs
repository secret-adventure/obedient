{-# LANGUAGE EmptyDataDecls #-}
module Graphics.Obedient.Internal where
import Diagrams.Core.Points
import Diagrams.TwoD.Types
import Control.Proxy.FRP 

data Behavior a

type Point2D = Point R2

clickLeft :: Event Point2D
clickLeft = undefined

clickRight :: Event Point2D
clickRight = undefined

clickMiddle :: Event Point2D
clickMiddle = undefined

mousePosition :: Behavior Point2D
mousePosition = undefined

data ScrollDirection 
   = ScrollUp
   | ScrollDown

mouseScroll :: Event ScrollDirection
mouseScroll = undefined





               