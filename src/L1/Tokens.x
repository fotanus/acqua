{
{-# OPTIONS -w  #-}
module L1.Tokens
  ( Token(..)
  , AlexPosn(..)
  , TokenClass(..)
  , Alex(..)
  , runAlex'
  , alexMonadScan'
  , alexError'
  ) where

import Prelude hiding (lex)
import Control.Monad ( liftM )
}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [A-Za-z]

tokens :-
  $white+                       ;
  let                           { lex' TokenLet }
  letrec                        { lex' TokenLetrec }
  in                            { lex' TokenIn }
  \\                            { lex' TokenLambda }
  \.                            { lex' TokenDot }
  \,                            { lex' TokenComma }
  if                            { lex' TokenIf }
  then                          { lex' TokenThen }
  else                          { lex' TokenElse }
  fn                            { lex' TokenFn }
  end                           { lex' TokenEnd }
  and	  	                  		{ lex' TokenAnd }
  or	  	                  		{ lex' TokenOr }
  int                           { lex' TokenInt }
  head                          { lex' TokenHead }
  tail                          { lex' TokenTail }
  last                          { lex' TokenLast }
  length                        { lex' TokenLength }
  concat                        { lex' TokenConcat }
  map                           { lex' TokenMap }
  slice                         { lex' TokenSlice }
  filter                        { lex' TokenFilter }
  $digit+                       { lex  (TokenNum . read) }
  \&\&		                  		{ lex' TokenAnd }
  \|\|		                  		{ lex' TokenOr }
  \=\>		                  		{ lex' TokenDoubleArrow }
  \-\>		                  		{ lex' TokenArrow }
  \>\=		                  		{ lex' TokenGE }
  \<\=		                  		{ lex' TokenLE }
  \<			                    	{ lex' TokenLT }
  \>			                    	{ lex' TokenGT }
  \!\=                          { lex' TokenNEQ }
  \=\=                          { lex' TokenEQ }
  \=                            { lex' TokenEQ }
  \:				                    { lex' TokenColon }
  \+                            { lex' TokenAdd }
  \-                            { lex' TokenSub }
  \*                            { lex' TokenMult }
  \/                            { lex' TokenDiv }
  \(                            { lex' TokenLParen }
  \)                            { lex' TokenRParen }
  \[                            { lex' TokenLBracket }
  \]                            { lex' TokenRBracket }
  $alpha [$alpha $digit \_ \']* { lex  TokenSym }

{
-- To improve error messages, We keep the path of the file we are
-- lexing in our own state.
data AlexUserState = AlexUserState { filePath :: FilePath }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>"

getFilePath :: Alex FilePath
getFilePath = liftM filePath alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath = alexSetUserState . AlexUserState

-- The token type, consisting of the source code position and a token class.
data Token = Token AlexPosn TokenClass
  deriving ( Show )

data TokenClass
           = TokenLet
           | TokenLetrec
           | TokenIn
           | TokenLambda
           | TokenDot
           | TokenComma
           | TokenIf
           | TokenThen
           | TokenElse
           | TokenFn
           | TokenEnd
           | TokenInt
           | TokenNum Int
           | TokenHead
           | TokenTail
           | TokenLast
           | TokenLength
           | TokenConcat
           | TokenMap
           | TokenSlice
           | TokenFilter
           | TokenSym String
           | TokenEQ
           | TokenNEQ
           | TokenColon
           | TokenDoubleArrow
           | TokenArrow
           | TokenAnd
           | TokenOr
           | TokenGE
           | TokenGT
           | TokenLE
           | TokenLT
           | TokenAdd
           | TokenSub
           | TokenMult
           | TokenDiv
           | TokenLParen
           | TokenRParen
           | TokenLBracket
           | TokenRBracket
           | TokenEOF
           deriving (Eq,Show)

alexEOF :: Alex Token
alexEOF = do
  (p,_,_,_) <- alexGetInput
  return $ Token p TokenEOF

-- Unfortunately, we have to extract the matching bit of string
-- ourselves...
lex :: (String -> TokenClass) -> AlexAction Token
lex f = \(p,_,_,s) i -> return $ Token p (f (take i s))

-- For constructing tokens that do not depend on
-- the input
lex' :: TokenClass -> AlexAction Token
lex' = lex . const

-- We rewrite alexMonadScan' to delegate to alexError' when lexing fails
-- (the default implementation just returns an error message).
alexMonadScan' :: Alex Token
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (p, _, _, s) ->
        alexError' p ("lexical error at character '" ++ take 1 s ++ "'")
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

-- Signal an error, including a commonly accepted source code position.
alexError' :: AlexPosn -> String -> Alex a
alexError' (AlexPn _ l c) msg = do
  fp <- getFilePath
  alexError (fp ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)

-- A variant of runAlex, keeping track of the path of the file we are lexing.
runAlex' :: Alex a -> String -> Either String a
runAlex' a input = runAlex input a
}
