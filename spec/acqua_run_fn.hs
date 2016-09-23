import Test.Hspec
import System.Directory
import System.Process
import Text.Regex.Posix
import Data.List.Split (splitOn)
import Debug.Trace


fixturesDir :: String
fixturesDir = "spec/fixtures/map/"

main :: IO ()
main = do
  files <- getDirectoryContents fixturesDir
  let cases =
            [
              ((fixturesDir++"sum.l1"), ["1", "2", "3"], "2, 3, 4"),
              ((fixturesDir++"fat.l1"), ["1", "2", "3", "4"], "1, 2, 6, 24"),
              ((fixturesDir++"head.l1"), ["[1,2,3]", "[2,3,4]", "[3,4,5]", "[4,5,6]"], "1, 2, 3, 4"),
              ((fixturesDir++"fibo.l1"), ["1", "2", "3", "4"], "1, 2, 3, 5")
            ]
  hspec (spec cases)

spec :: [(String,[String],String)] -> Spec
spec [] = do return ()
spec ((file,args,correctResult):ss) = do
  it file $ do
    fileContents <- readFile file
    output <- readProcess "./acqua-run" (["10", "0", "x"] ++ args) fileContents
    result <- return $ drop (length "response: ") (output =~ "response: ([0-9]+, )+" :: String)
    result `shouldBe` (correctResult ++ ", ")

  it (file ++ " with hierarchical crossbar") $ do
    fileContents <- readFile file
    output <- readProcess "./acqua-run" (["8", "4", "x"] ++ args) fileContents
    result <- return $ drop (length "response: ") (output =~ "response: ([0-9]+, )+" :: String)
    result `shouldBe` (correctResult ++ ", ")
  spec ss
