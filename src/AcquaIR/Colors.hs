module AcquaIR.Colors where

blue :: String -> String
blue s = "\x1b[34m" ++ s ++ "\x1b[0m"

yellow :: String -> String
yellow s = "\x1b[33m" ++ s ++ "\x1b[0m"

green :: String -> String
green s = "\x1b[32m" ++ s ++ "\x1b[0m"

red :: String -> String
red s = "\x1b[31m" ++ s ++ "\x1b[0m"

cyan :: String -> String
cyan s = "\x1b[35m" ++ s ++ "\x1b[0m"

