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
  let fixtures = (filter (\f -> not (elem f [".", ".."])) files)
  hspec (spec fixtures)

spec :: [String] -> Spec
spec [] = do return ()

spec (s:ss) = do
  it s $ do (runFile (fixturesDir ++ s)) `shouldReturn` "2, 3, 4, "
  spec ss

runFile file = do
  fileContents <- readFile file
  output <- readProcess "./acqua-run-map" ["10","x","1","2","3"] fileContents
  return $ last $ (splitOn " " (output =~ "response: [0-9, ]+" :: String))
  return $  drop (length "response: ") (output =~ "response: ([0-9]+, )+" :: String)
