module Main where
import Debug.Trace
import Text.Show.Pretty

import L1.Grammar
import AcquaIR.Language
import AcquaIR.Compile
import Simulator.Acqua
import Simulator.Rules
import Simulator.Rules.Base

main :: IO ()
main = do
  s <- getContents
  let eitherAst = parse s
  case eitherAst of
    Right ast -> do
      -- putStrLn $ run (compile ast) 1
      putStrLn $ run p 2
    Left errorMsg ->
      putStrLn errorMsg
  where
    run :: Program -> Int -> String
    run program pus_n = step (newAcqua program pus_n)

    applyRules :: [Rule] -> Acqua -> Acqua
    applyRules [] a = a
    applyRules (f:fs) a = applyRules fs (f a)

    step :: Acqua -> String
    step acqua = _step (applyRules rules acqua) acqua

    _step :: Acqua -> Acqua -> String
    _step (Acqua _ _ pus _ True _) _ = "Finished!\n" ++ (ppShow (head pus))
    _step acqua acqua'=
      if acqua == acqua'
        then error ("Cannot give a step!\n" ++ (ppShow acqua))
        else trace ("----") $ _step (applyRules rules (untaintAll acqua)) acqua

    p = [ BB
              { label = "main"
              , size = 0
              , commands =
                  [ AssignL "fibo" "_fn_0"
                  , AssignV "resp" "fibo"
                  , AssignV "fn" "resp"
                  , AssignI "resp" 7
                  , EnvNew "env_id3" 0
                  , EnvAddL "env_id3" "x" "resp"
                  , EnvAddL "env_id3" "fibo" "fibo"
                  , Call "resp" "fn" "env_id3"
                  , Wait
                  ]
              , terminator = Return "resp"
              }
          , BB
              { label = "_fn_0"
              , size = 0
              , commands =
                  [ AssignV "resp" "x"
                  , AssignV "var0" "resp"
                  , AssignI "resp" 2
                  , AssignV "var1" "resp"
                  , Op "var0" Lesser "var1"
                  ]
              , terminator = If "resp" "then0"
              }
          , BB
              { label = "dummy0"
              , size = 0
              , commands =
                  [ AssignV "resp" "fibo"
                  , AssignV "fn" "resp"
                  , AssignV "resp" "x"
                  , AssignV "var2" "resp"
                  , AssignI "resp" 1
                  , AssignV "var3" "resp"
                  , Op "var2" Sub "var3"
                  , AssignV "x1" "resp"
                  , EnvNew "env_id1" 0
                  , EnvAddL "env_id1" "x" "x1"
                  , EnvAddL "env_id1" "fibo" "fibo"
                  , Call "var6" "fn" "env_id1"
                  , AssignV "resp" "fibo"
                  , AssignV "fn" "resp"
                  , AssignV "resp" "x"
                  , AssignV "var4" "resp"
                  , AssignI "resp" 2
                  , AssignV "var5" "resp"
                  , Op "var4" Sub "var5"
                  , AssignV "x2" "resp"
                  , EnvNew "env_id2" 0
                  , EnvAddL "env_id2" "x" "x2"
                  , EnvAddL "env_id2" "fibo" "fibo"
                  , Call "var7" "fn" "env_id2"
                  , Wait
                  , Op "var6" Add "var7"
                  ]
              , terminator = Goto "back0"
              }
          , BB
              { label = "back0"
              , size = 0
              , commands = []
              , terminator = Return "resp"
              }
          , BB
              { label = "then0"
              , size = 0
              , commands = [ AssignI "resp" 1 ]
              , terminator = Goto "back0"
              }
          ]

