{
module L1.Grammar where
import L1.Tokens
import L1.Language
}

%name parseL1
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEOF }
%error { parseError }

%token
    let    { Token _ TokenLet }
    letrec { Token _ TokenLetrec }
    in     { Token _ TokenIn }
    fn     { Token _ TokenFn }
    end    { Token _ TokenEnd }
    if     { Token _ TokenIf }
    then   { Token _ TokenThen }
    else   { Token _ TokenElse }
    num    { Token _ (TokenNum $$) }
    var    { Token _ (TokenSym $$) }
    '!='   { Token _ TokenNEQ }
    'and'  { Token _ TokenAnd }
    'or'   { Token _ TokenOr }
    '='    { Token _ TokenEQ }
    '=='   { Token _ TokenEQ }
    '+'    { Token _ TokenAdd }
    '-'    { Token _ TokenSub }
    '*'    { Token _ TokenMult }
    '('    { Token _ TokenLParen }
    ')'    { Token _ TokenRParen }
    '=>'   { Token _ TokenDoubleArrow }
    '>='   { Token _ TokenGE }
    '<='   { Token _ TokenLE }
    '>'    { Token _ TokenGT }
    '<'    { Token _ TokenLT }

%left '=>'
%left if then else
%left '<' '>' '!=' '==' '<=' '>=' '=' 'and' 'or'
%left '+' '-'
%left '*'
%left '(' ')'
%left let letrec
%left APP
%left NOT_APP
%left NEGATIVE

%%
Exp :
    -- Let and letrec
    let var '=' Exp in Exp end           { Let $2 $4 $6 }
    | letrec var '=' Exp in Exp end      { Letrec $2 $4 $6 }

    -- Constructions
    | if BoolExp then Exp else Exp       { If $2 $4 $6 }

    -- Math
    | Exp '+' Exp                        { Op $1 Add $3 }
    | Exp '-' Exp                        { Op $1 Sub $3 }
    | Exp '*' Exp                        { Op $1 Mult $3 }

    -- Literals
    | var %prec NOT_APP                  { Ident $1 }
    | fn var '=>' Exp                    { Fn $2 $4 }
    | num                                { Num $1 }
    | '-' num %prec NEGATIVE             { Num (-$2) }
    | '(' Exp ')'                        { $2 }

    -- Application
    | var Exp   %prec APP                          { App (Ident $1) $2 }
    | '(' Exp ')' Exp %prec APP                    { App $2 $4 }


BoolExp : Exp '==' Exp          { Op $1 Equal $3 }
        | Exp '!=' Exp          { Op $1 NotEqual $3 }
        | Exp '>=' Exp          { Op $1 GreaterEqual $3 }
        | Exp '<=' Exp          { Op $1 LesserEqual $3 }
        | Exp '>' Exp           { Op $1 Greater $3 }
        | Exp '<' Exp           { Op $1 Lesser $3 }
        | BoolExp 'or' BoolExp  { Op $1 Or $3 }
        | BoolExp 'and' BoolExp { Op $1 And $3 }
        | '(' BoolExp ')'       { $2 }

{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

parseError :: Token -> Alex a
parseError (Token p t) = alexError' p ("parse error at token '" ++ show t ++ "'")


parse:: String -> Either String Term
parse = runAlex' parseL1
}
