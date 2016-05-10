import Test.Hspec

import L1.Language as L1
import UL1.Language as UL1
import UL1.FromL1

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "ident x" $ do
    fromL1 (L1.Ident "x")
    `shouldBe`
    UL1.Ident "x"

  it "error" $ do
    fromL1 (L1.Ident "x")
    `shouldBe`
    UL1.Ident "y"

