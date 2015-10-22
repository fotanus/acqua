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
    int    { Token _ TokenInt }
    num    { Token _ (TokenNum $$) }
    var    { Token _ (TokenSym $$) }
    '!='   { Token _ TokenNEQ }
    '&&'   { Token _ TokenAnd }
    '||'   { Token _ TokenOr }
    'and'  { Token _ TokenAnd }
    'or'   { Token _ TokenOr }
    '\\'   { Token _ TokenLambda }
    '.'    { Token _ TokenDot }
    '='    { Token _ TokenEQ }
    '=='   { Token _ TokenEQ }
    '+'    { Token _ TokenAdd }
    '-'    { Token _ TokenSub }
    '*'    { Token _ TokenMult }
    '('    { Token _ TokenLParen }
    ')'    { Token _ TokenRParen }
    ':'    { Token _ TokenColon }
    '->'   { Token _ TokenArrow }
    '=>'   { Token _ TokenDoubleArrow }
    '>='   { Token _ TokenGE }
    '<='   { Token _ TokenLE }
    '>'    { Token _ TokenGT }
    '<'    { Token _ TokenLT }

%left ':' '=>' '.'
%left '->'
%left if then else
%left '||' '&&' '<' '>' '!=' '==' '<=' '>=' '=' 'and' 'or'
%left '+' '-'
%left '*'
%left '(' ')'
%left fn in '\\'
%left let letrec
%left NEGATIVE_NUMBER

%%

ExpBase : LetExp          { $1 }
        | Exp             { $1 }
        | '(' ExpBase ')' { $2 }

LetExp : let var '=' Exp in ExpBase end                     { Let $2 $4 $6 }
       | let var '=' Exp in ExpBase                         { Let $2 $4 $6 }
       | let var ':' Type '=' Exp in ExpBase end            { Let $2 $6 $8 }
       | letrec var ':' Type '=' '(' Fn ')' in ExpBase end  { Letrec $2 $7 $10 }
       | letrec var '=' '(' Fn ')' in ExpBase end           { Letrec $2 $5 $8 }
       | letrec var '=' Fn in ExpBase end                   { Letrec $2 $4 $6 }
       | letrec var '=' Fn in ExpBase                       { Letrec $2 $4 $6 }
       | '(' LetExp ')' { $2 }


Exp : num                                       { Num $1 }
    | '-' num %prec NEGATIVE_NUMBER             { Num (-$2) }
    | Exp OpCode Exp                            { Op $1 $2 $3 }
    | '(' Exp ')'                               { $2 }
    | '(' OpCode Exp Exp ')'                    { Op $3 $2 $4 }
    | Fn                                        { $1 }
    | var                                       { Ident $1 }
    | if ExpBase then ExpBase else ExpBase      { If $2 $4 $6 }
    | Exp num                                   { App $1 (Num $2) }
    | Exp '(' Exp ')'                           { App $1 ($3) }
    | Exp '(' OpCode Exp Exp ')'                { App $1 (Op $4 $3 $5) }
    | Exp Fn                                    { App $1 $2 }
    | Exp var                                   { App $1 (Ident $2) }
    | Exp if ExpBase then ExpBase else ExpBase  { App $1 (If $3 $5 $7) }

Fn : fn var ':' Type '=>' Exp   { Fn $2 $6 }
   | fn var '=>' Exp            { Fn $2 $4 }
   | '\\' var '.' ExpBase       { Fn $2 $4 }

Type : int             { }
     | Type '->' Type  { }

OpCode  : '+'   { Add }
        | '-'   { Sub }
        | '*'   { Mult }
        | '='   { Equal }
        | '=='  { Equal }
        | '!='  { NotEqual }
        | '>='  { GreaterEqual }
        | '<='  { LesserEqual }
        | '>'   { Greater }
        | '<'   { Lesser }
        | '||'  { Or }
        | 'or'  { Or }
        | '&&'  { And }
        | 'and' { And }

{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

parseError :: Token -> Alex a
parseError (Token p t) = alexError' p ("parse error at token '" ++ show t ++ "'")


parse:: String -> Either String Term
parse = runAlex' parseL1
}
