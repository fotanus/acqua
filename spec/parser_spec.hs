import Test.Hspec

import L1.Language
import L1.Grammar
import L1.Type

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
    Right (Let "x" (Num 3 defaultAnnotations) (Ident "x" defaultAnnotations) defaultAnnotations)

  it "sum" $ do
    parse "1 + 2"
    `shouldBe`
    Right (Op (Num 1 defaultAnnotations) Add (Num 2 defaultAnnotations) defaultAnnotations)

  it "multiplication precedence" $ do
    parse "1 + 2 * 4"
    `shouldBe`
    Right (Op (Num 1 defaultAnnotations) Add (Op (Num 2 defaultAnnotations) Mult (Num 4 defaultAnnotations) defaultAnnotations) defaultAnnotations)

  it "parenthesis" $ do
    parse "1 + (2 + 3)"
    `shouldBe`
    Right (Op (Num 1 defaultAnnotations) Add (Op (Num 2 defaultAnnotations) Add (Num 3 defaultAnnotations) defaultAnnotations) defaultAnnotations)

  it "fn" $ do
    parse "fn x => 1"
    `shouldBe`
    Right (Fn ["x"] (Num 1 defaultAnnotations) defaultAnnotations)

  it "if" $ do
    parse "if x < 4 then 5 else x"
    `shouldBe`
    Right (If (Op (Ident "x" defaultAnnotations) Lesser (Num 4 defaultAnnotations) defaultAnnotations) (Num 5 defaultAnnotations) (Ident "x" defaultAnnotations) defaultAnnotations)

  it "let rec" $ do
    parse "letrec zero = fn x => 1 in 4 end"
    `shouldBe`
    Right (Letrec "zero" (Fn ["x"] (Num 1 defaultAnnotations)  defaultAnnotations) (Num 4 defaultAnnotations) defaultAnnotations)

  it "var minus number" $ do
    parse "x - 4"
    `shouldBe`
    Right (Op (Ident "x" defaultAnnotations) Sub (Num 4 defaultAnnotations) defaultAnnotations)

  it "fn subtracting number" $ do
    parse "fn x => 1 - 4"
    `shouldBe`
    Right (Fn ["x"] (Op (Num 1 defaultAnnotations) Sub (Num 4 defaultAnnotations) defaultAnnotations) defaultAnnotations)

  it "application" $ do
    parse "(fn x => x) 1"
    `shouldBe`
    Right (App (Fn ["x"] (Ident "x" defaultAnnotations) defaultAnnotations) (Num 1 defaultAnnotations) defaultAnnotations)

  it "var application" $ do
    parse "x 1"
    `shouldBe`
    Right (App (Ident "x" defaultAnnotations) (Num 1 defaultAnnotations) defaultAnnotations)

  it "currying" $ do
    parse "fn x => fn y => fn z => 1"
    `shouldBe`
    Right (Fn ["x"] (Fn ["y"] (Fn ["z"] (Num 1 defaultAnnotations) defaultAnnotations) defaultAnnotations) defaultAnnotations)

  it "high order function" $ do
    parse "fn x => fn y => x y"
    `shouldBe`
    Right (Fn ["x"] (Fn ["y"] (App (Ident "x" defaultAnnotations) (Ident "y" defaultAnnotations) defaultAnnotations) defaultAnnotations) defaultAnnotations)

  it "nested if on then" $ do
    parse "if x > 10 then if y > 11 then 1 else 2 else 3"
    `shouldBe`
    Right (If
            (Op (Ident "x" defaultAnnotations) Greater (Num 10 defaultAnnotations) defaultAnnotations)
            (If (Op (Ident "y" defaultAnnotations) Greater (Num 11 defaultAnnotations) defaultAnnotations) (Num 1 defaultAnnotations) (Num 2 defaultAnnotations) defaultAnnotations)
            (Num 3 defaultAnnotations)
           defaultAnnotations)

  it "nested if on else" $ do
    parse "if x > 10 then 3 else if y > 11 then 1 else 2"
    `shouldBe`
    Right (If
            (Op (Ident "x" defaultAnnotations) Greater (Num 10 defaultAnnotations) defaultAnnotations)
            (Num 3 defaultAnnotations)
            (If (Op (Ident "y" defaultAnnotations) Greater (Num 11 defaultAnnotations) defaultAnnotations) (Num 1 defaultAnnotations) (Num 2 defaultAnnotations) defaultAnnotations)
           defaultAnnotations)

  it "multi paramater function" $ do
    parse "fn x, y, z => x "
    `shouldBe`
    Right (Fn ["x", "y", "z"] (Ident "x" defaultAnnotations) defaultAnnotations )

  it "list" $ do
    parse "[1,2,3]"
    `shouldBe`
    Right (List [ListNum 1,ListNum 2,ListNum 3] defaultAnnotations)

  it "empty list" $ do
    parse "[]"
    `shouldBe`
    Right (List [] defaultAnnotations)

  it "one element list" $ do
    parse "[300]"
    `shouldBe`
    Right (List [ListNum 300] defaultAnnotations)

  it "length" $ do
    parse "length [300]"
    `shouldBe`
    Right (Length (List [ListNum 300] defaultAnnotations) defaultAnnotations)

  it "list with variable" $ do
    parse "[x]"
    `shouldBe`
    Right (List [ListIdent "x"] defaultAnnotations)

  it "slice" $ do
    parse "slice([], 0, 1)"
    `shouldBe`
    Right (Slice (List [] defaultAnnotations) (Num 0 defaultAnnotations) (Num 1 defaultAnnotations) defaultAnnotations)
