import Test.Hspec
import System.Directory
import System.Process
import Text.Regex.Posix
import Data.List.Split (splitOn)
import Debug.Trace


fixturesDir :: String
fixturesDir = "spec/fixtures/return_42/"


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "app_fn.l1" $ do
        (runFile (fixturesDir ++ "app_fn.l1")) `shouldReturn` "42, "
    it "app.l1" $ do
        (runFile (fixturesDir ++ "app.l1")) `shouldReturn` "42, "
    it "curry.l1" $ do
        (runFile (fixturesDir ++ "curry.l1")) `shouldReturn` "42, "
    it "fat.l1" $ do
        (runFile (fixturesDir ++ "fat.l1")) `shouldReturn` "42, "
    it "filter.l1" $ do
        (runFile (fixturesDir ++ "filter.l1")) `shouldReturn` "42, "
    it "head.l1" $ do
        (runFile (fixturesDir ++ "head.l1")) `shouldReturn` "42, "
    it "length.l1" $ do
        (runFile (fixturesDir ++ "length.l1")) `shouldReturn` "42, "
    it "let_curry.l1" $ do
        (runFile (fixturesDir ++ "let_curry.l1")) `shouldReturn` "42, "
    it "list_with_var.l1" $ do
        (runFile (fixturesDir ++ "list_with_var.l1")) `shouldReturn` "42, "
    it "multiparam.l1" $ do
        (runFile (fixturesDir ++ "multiparam.l1")) `shouldReturn` "42, "
    it "newton-raphson.l1" $ do
        (runFile (fixturesDir ++ "newton-raphson.l1")) `shouldReturn` "42, "
    it "simple_if.l1" $ do
        (runFile (fixturesDir ++ "simple_if.l1")) `shouldReturn` "42, "
    it "tail.l1" $ do
        (runFile (fixturesDir ++ "tail.l1")) `shouldReturn` "42, "
    it "app_fn_let.l1" $ do
        (runFile (fixturesDir ++ "app_fn_let.l1")) `shouldReturn` "42, "
    it "concat.l1" $ do
        (runFile (fixturesDir ++ "concat.l1")) `shouldReturn` "42, "
    it "double_if.l1" $ do
        (runFile (fixturesDir ++ "double_if.l1")) `shouldReturn` "42, "
    it "fibo.l1" $ do
        (runFile (fixturesDir ++ "fibo.l1")) `shouldReturn` "42, "
    it "freevar.l1" $ do
        (runFile (fixturesDir ++ "freevar.l1")) `shouldReturn` "42, "
    it "last.l1" $ do
        (runFile (fixturesDir ++ "last.l1")) `shouldReturn` "42, "
    it "let_curry_3.l1" $ do
        (runFile (fixturesDir ++ "let_curry_3.l1")) `shouldReturn` "42, "
    it "let_fn.l1" $ do
        (runFile (fixturesDir ++ "let_fn.l1")) `shouldReturn` "42, "
    it "map.l1" $ do
        (runFile (fixturesDir ++ "map.l1")) `shouldReturn` "42, "
    it "mult.l1" $ do
        (runFile (fixturesDir ++ "mult.l1")) `shouldReturn` "42, "
    it "simple_apply.l1" $ do
        (runFile (fixturesDir ++ "simple_apply.l1")) `shouldReturn` "42, "
    it "sum.l1" $ do
        (runFile (fixturesDir ++ "sum.l1")) `shouldReturn` "42, "
    it "use_cache.l1" $ do
        (runFile (fixturesDir ++ "use_cache.l1")) `shouldReturn` "42, "
    it "slice.l1" $ do
        (runFile (fixturesDir ++ "slice.l1")) `shouldReturn` "42, "



runFile file = do
  fileContents <- readFile file
  output <- readProcess "./acqua-run" ["10", "0", "foovar", "0"] fileContents
  return $ drop (length "response: ") (output =~ "response: ([0-9]+, )+" :: String)
