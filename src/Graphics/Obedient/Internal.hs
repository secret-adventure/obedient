{-# LANGUAGE EmptyDataDecls #-}
module Graphics.Obedient.Internal where

data Behavior a 
data Event a 
data Point

clickLeft :: Event Point
clickLeft = undefined

clickRight :: Event Point
clickRight = undefined

clickMiddle :: Event Point
clickMiddle = undefined

mousePosition :: Behavior Point
mousePosition = undefined

data ScrollDirection 
   = ScrollUp
   | ScrollDown

mouseScroll :: Event ScrollDirection
mouseScroll = undefined





               