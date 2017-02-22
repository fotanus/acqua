{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import L1.Grammar
import Compiler.Compile
import Simulator.Run
import System.Console.CmdArgs


data Opts = Opts {
  optimizations :: Int,
  pus  :: Int,
  nsteps :: Int,
  var :: String,
  val :: String
} deriving (Data,Typeable,Show)

-- Run a L1 program in a simulation
main :: IO ()
main = do
  opts <- cmdArgs $ Opts 3 1 0 "foovar" "0"
  s <- getContents
  let eitherAst = parse s
  case eitherAst of
    Right ast -> do
      putStrLn $ run (compile ast (optimizations opts)) (optimizations opts) (pus opts) (nsteps opts) (var opts) [(val opts)]
    Left errorMsg ->
      putStrLn errorMsg
