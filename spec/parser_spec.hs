import Test.Hspec

import L1.Language
import L1.Grammar

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "error" $ do
    parse "let x = 3 x" `shouldBe` Left "<unknown>:1:12: parse error at token 'TokenEOF'"

  it "let" $ do
    parse "let x = 3 in x" `shouldBe` Right (Let "x" (Num 3) (Ident "x"))

