module Internal where
import Graphics.Obedient.Internal
import Control.Monad.State
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.Framework
import Test.HUnit hiding (test)

case_clickLeft = assertFailure "clickLeft"

tests = $testGroupGenerator