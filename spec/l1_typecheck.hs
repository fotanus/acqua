import Test.Hspec
import System.Directory
import System.Process
import Text.Regex.Posix
import Data.List.Split (splitOn)


fixturesDir :: String
fixturesDir = "spec/fixtures/run/"

main :: IO ()
main = do
  files <- getDirectoryContents fixturesDir
  let fixtures = (filter (\f -> not (elem f [".", "..", "map"])) files)
  hspec (spec fixtures)

spec :: [String] -> Spec
spec [] = do return ()

spec (s:ss) = do
  it s $ do (runFile (fixturesDir ++ s)) `shouldReturn` "IntT\n"
  spec ss

runFile file = do
  fileContents <- readFile file
  output <- readProcess "./l1-typecheck" [] fileContents
  return $ output
