module AcquaIR.Colors where

blue :: String -> String
blue s = "\x1b[34m" ++ s ++ "\x1b[0m"

yellow :: String -> String
yellow s = "\x1b[33m" ++ s ++ "\x1b[0m"

green :: String -> String
green s = "\x1b[32m" ++ s ++ "\x1b[0m"

