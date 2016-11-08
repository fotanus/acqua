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
    head   { Token _ TokenHead }
    tail   { Token _ TokenTail }
    last   { Token _ TokenLast }
    length { Token _ TokenLength }
    concat { Token _ TokenConcat }
    map    { Token _ TokenMap }
    slice  { Token _ TokenSlice }
    filter { Token _ TokenFilter }
    '!='   { Token _ TokenNEQ }
    'and'  { Token _ TokenAnd }
    'or'   { Token _ TokenOr }
    '='    { Token _ TokenEQ }
    '=='   { Token _ TokenEQ }
    ','    { Token _ TokenComma }
    '+'    { Token _ TokenAdd }
    '-'    { Token _ TokenSub }
    '*'    { Token _ TokenMult }
    '/'    { Token _ TokenDiv }
    '('    { Token _ TokenLParen }
    ')'    { Token _ TokenRParen }
    '['    { Token _ TokenLBracket }
    ']'    { Token _ TokenRBracket }
    '=>'   { Token _ TokenDoubleArrow }
    '>='   { Token _ TokenGE }
    '<='   { Token _ TokenLE }
    '>'    { Token _ TokenGT }
    '<'    { Token _ TokenLT }

%left '=>'
%left ','
%left if then else
%left '<' '>' '!=' '==' '<=' '>=' '=' 'and' 'or'
%left '+' '-'
%left '*' '/'
%left head tail last length
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
    | if Exp then Exp else Exp       { If $2 $4 $6 }

    -- Math
    | Exp '+' Exp                        { Op $1 Add $3 }
    | Exp '-' Exp                        { Op $1 Sub $3 }
    | Exp '*' Exp                        { Op $1 Mult $3 }
    | Exp '/' Exp                        { Op $1 Div $3 }
    | Exp '=' Exp                       { Op $1 Equal $3 }
    | Exp '!=' Exp                       { Op $1 NotEqual $3 }
    | Exp '>=' Exp                       { Op $1 GreaterEqual $3 }
    | Exp '<=' Exp                       { Op $1 LesserEqual $3 }
    | Exp '>' Exp                        { Op $1 Greater $3 }
    | Exp '<' Exp                        { Op $1 Lesser $3 }
    | Exp 'or' Exp                       { Op $1 Or $3 }
    | Exp 'and' Exp                      { Op $1 And $3 }

    -- Literals
    | var %prec NOT_APP                  { Ident $1 }
    | fn vars '=>' Exp                   { Fn $2 $4 [] }
    | num                                { Num $1 }
    | '-' num %prec NEGATIVE             { Num (-$2) }
    | '(' Exp ')'                        { $2 }

    -- Lists
    | '[' ']'                            { List [] }
    | '[' nums ']'                       { List $2 }
    | head Exp                           { Head $2 }
    | tail Exp                           { Tail $2 }
    | last Exp                           { Last $2 }
    | length Exp                         { Length $2 }
    | concat '(' Exp ',' Exp ')'         { Concat $3 $5 }
    | map '(' Exp ',' Exp ')'            { Map $3 $5 }
    | slice '(' Exp ',' Exp ',' Exp ')'  { Slice $3 $5 $7 }
    | filter '(' Exp ',' Exp ')'         { Filter $3 $5 }

    -- Application
    | var Exp   %prec APP                          { App (Ident $1) $2 }
    | '(' Exp ')' Exp %prec APP                    { App $2 $4 }

vars : var ',' vars   { [$1] ++ $3 }
     | var            { [$1] }

nums : num ',' nums             { [ListNum $1] ++ $3 }
     | var ',' nums             { [ListIdent $1] ++ $3 }
     | '[' ']' ',' nums         { [RecList []] ++ $4 }
     | '[' nums ']' ',' nums    { [RecList $2] ++ $5 }
     | num                      { [ListNum $1] }
     | var                      { [ListIdent $1] }
     | '[' ']'                  { [RecList []] }
     | '[' nums ']'             { [RecList $2] }


{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

parseError :: Token -> Alex a
parseError (Token p t) = alexError' p ("parse error at token '" ++ show t ++ "'")


parse:: String -> Either String Term
parse = runAlex' parseL1
}
