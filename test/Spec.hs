import Test.Hspec

import qualified ParserSpec
import qualified ParserDeclSpec
import qualified InferenceSpec
import qualified EvalSpec
import qualified KindSpec
import qualified TypeSynonymSpec
import qualified ModuleSpec
import qualified ErrorSpec

main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "Parsing tests" $ do
    ParserSpec.spec
    ParserDeclSpec.spec

  describe "Type inference tests" $ do
    InferenceSpec.spec

  describe "Kind inference tests" $ do
    KindSpec.spec

  describe "Type synonym tests" $ do
    TypeSynonymSpec.spec

  describe "Module analysis tests" $ do
    ModuleSpec.spec

  describe "Evaluation tests" $ do
    EvalSpec.spec

  describe "Error handling tests" $ do
    ErrorSpec.spec