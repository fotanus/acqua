import Test.Hspec

import L1.Language as L1
import UL1.Language as UL1
import UL1.FromL1

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "remove free variables form function" $ do
    fromL1 (L1.Fn "x" (L1.Op (L1.Ident "x") L1.Add (L1.Ident "y")))
    `shouldBe`
    UL1.Fn ["x","y"] (UL1.Op (UL1.Ident "x") UL1.Add (UL1.Ident "y"))
