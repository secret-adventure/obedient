module Main where
import Internal as I 
import Control.Applicative
import Data.Monoid
import Test.Framework.Options
import Test.Framework

main = defaultMain [
        I.tests
    ] 