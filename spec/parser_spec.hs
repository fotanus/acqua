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
    Left "<unknown>:1:11: parse error at token 'TokenSym \"x\"'"

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
    Right (Fn "x" (Num 1) [])

  it "if" $ do
    parse "if x < 4 then 5 else x"
    `shouldBe`
    Right (If (Op (Ident "x") Lesser (Num 4)) (Num 5) (Ident "x"))

  it "let rec" $ do
    parse "letrec zero = fn x => 1 in 4 end"
    `shouldBe`
    Right (Letrec "zero" (Fn "x" (Num 1) []) (Num 4))

  it "var minus number" $ do
    parse "x - 4"
    `shouldBe`
    Right (Op (Ident "x") Sub (Num 4))

  it "fn subtracting number" $ do
    parse "fn x => 1 - 4"
    `shouldBe`
    Right (Fn "x" (Op (Num 1) Sub (Num 4)) [])

  it "application" $ do
    parse "(fn x => x) 1"
    `shouldBe`
    Right (App (Fn "x" (Ident "x") []) (Num 1))

  it "var application" $ do
    parse "x 1"
    `shouldBe`
    Right (App (Ident "x") (Num 1))

  it "currying" $ do
    parse "fn x => fn y => fn z => 1"
    `shouldBe`
    Right (Fn "x" (Fn "y" (Fn "z" (Num 1) []) []) [])

  it "high order function" $ do
    parse "fn x => fn y => x y"
    `shouldBe`
    Right (Fn "x" (Fn "y" (App (Ident "x") (Ident "y")) []) [])

  it "nested if on then" $ do
    parse "if x > 10 then if y > 11 then 1 else 2 else 3"
    `shouldBe`
    Right (If
            (Op (Ident "x") Greater (Num 10))
            (If (Op (Ident "y") Greater (Num 11)) (Num 1) (Num 2))
            (Num 3)
          )

  it "nested if on else" $ do
    parse "if x > 10 then 3 else if y > 11 then 1 else 2"
    `shouldBe`
    Right (If
            (Op (Ident "x") Greater (Num 10))
            (Num 3)
            (If (Op (Ident "y") Greater (Num 11)) (Num 1) (Num 2))
          )



