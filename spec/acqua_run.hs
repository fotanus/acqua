import Test.Hspec
import System.Directory
import System.Process
import Text.Regex.Posix
import Data.List.Split (splitOn)
import Debug.Trace


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
  it s $ do (runFile (fixturesDir ++ s)) `shouldReturn` "42"
  spec ss

runFile file = do
  fileContents <- readFile file
  output <- readProcess "./acqua-run" ["10"] fileContents
  return $ last $ (splitOn " " (output =~ "response: NumberV [0-9]+" :: String))
