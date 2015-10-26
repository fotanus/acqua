import Test.Hspec

import L1.Language
import L1.Grammar

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "error" $ do
    parse "let x = 3 x"
    `shouldBe`
    Left "<unknown>:1:12: parse error at token 'TokenEOF'"

  it "let" $ do
    parse "let x = 3 in x end"
    `shouldBe`
    Right (Let "x" (Num 3) (Ident "x"))

  it "sum" $ do
    parse "1 + 2"
    `shouldBe`
    Right (Op (Num 1) Add (Num 2))

  it "multiplication precedence" $ do
    parse "1 + 2 * 4"
    `shouldBe`
    Right (Op (Num 1) Add (Op (Num 2) Mult (Num 4)))

  it "parenthesis" $ do
    parse "1 + (2 + 3)"
    `shouldBe`
    Right (Op (Num 1) Add (Op (Num 2) Add (Num 3)))

  it "fn" $ do
    parse "fn x => 1"
    `shouldBe`
    Right (Fn "x" (Num 1))

  it "if" $ do
    parse "if x < 4 then 5 else x"
    `shouldBe`
    Right (If (Op (Ident "x") Lesser (Num 4)) (Num 5) (Ident "x"))

  it "let rec" $ do
    parse "letrec zero = fn x => x - 1 in 4 end"
    `shouldBe`
    Right (Letrec "zero" (Fn "x" (Op (Ident "x") Sub (Num 1))) (Num 4))

  it "app" $ do
    parse "(fn x => x) 1"
    `shouldBe`
    Right (App (Fn "x" (Ident "x")) (Num 1))



