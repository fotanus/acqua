import Test.Hspec
import System.Directory
import System.Process
import Text.Regex.Posix
import Data.List.Split (splitOn)


fixturesDir :: String
fixturesDir = "spec/fixtures/"

main :: IO ()
main = do
  files <- getDirectoryContents fixturesDir
  let fixtures = (filter (\f -> not (elem f [".", ".."])) files)
  hspec (spec fixtures)

spec :: [String] -> Spec
spec [] = do return ()

spec (s:ss) = do
  it s $ do (runFile (fixturesDir ++ s)) `shouldReturn` "42"
  spec ss

runFile file = do
  fileContents <- readFile file
  output <- readProcess "./acqua-run" ["10"] fileContents
  return $ last $ (splitOn " " (output =~ "\"result\" , NumberValue [0-9]+" :: String))
