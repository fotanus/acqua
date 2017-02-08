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
    concat3 { Token _ TokenConcat3 }
    map    { Token _ TokenMap }
    'mod'  { Token _ TokenMod }
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
    let var '=' Exp in Exp end           { Let $2 $4 $6 defaultAnnotations }
    | letrec var '=' Exp in Exp end      { Letrec $2 $4 $6 defaultAnnotations }

    -- Constructions
    | if Exp then Exp else Exp       { If $2 $4 $6 defaultAnnotations }

    -- Math
    | Exp '+' Exp                        { Op $1 Add $3 defaultAnnotations }
    | Exp '-' Exp                        { Op $1 Sub $3 defaultAnnotations }
    | Exp '*' Exp                        { Op $1 Mult $3 defaultAnnotations }
    | Exp '/' Exp                        { Op $1 Div $3 defaultAnnotations }
    | Exp '=' Exp                       { Op $1 Equal $3 defaultAnnotations }
    | Exp '!=' Exp                       { Op $1 NotEqual $3 defaultAnnotations }
    | Exp '>=' Exp                       { Op $1 GreaterEqual $3 defaultAnnotations }
    | Exp '<=' Exp                       { Op $1 LesserEqual $3 defaultAnnotations }
    | Exp 'mod' Exp                      { Op $1 LesserEqual $3 defaultAnnotations }
    | Exp '>' Exp                        { Op $1 Greater $3 defaultAnnotations }
    | Exp '<' Exp                        { Op $1 Lesser $3 defaultAnnotations }
    | Exp 'or' Exp                       { Op $1 Or $3 defaultAnnotations }
    | Exp 'and' Exp                      { Op $1 And $3 defaultAnnotations }

    -- Literals
    | var %prec NOT_APP                  { Ident $1 defaultAnnotations }
    | fn vars '=>' Exp                   { Fn $2 $4 defaultAnnotations }
    | num                                { Num $1 defaultAnnotations }
    | '-' num %prec NEGATIVE             { Num (-$2) defaultAnnotations }
    | '(' Exp ')'                        { $2 }

    -- Lists
    | '[' ']'                            { List [] defaultAnnotations }
    | '[' nums ']'                       { List $2 defaultAnnotations }
    | head Exp                           { Head $2 defaultAnnotations }
    | tail Exp                           { Tail $2 defaultAnnotations }
    | last Exp                           { Last $2 defaultAnnotations }
    | length Exp                         { Length $2 defaultAnnotations }
    | concat '(' Exp ',' Exp ')'         { Concat $3 $5 defaultAnnotations }
    | concat3 '(' Exp ',' Exp ',' Exp ')'         { Concat3 $3 $5 $7 defaultAnnotations }
    | map '(' Exp ',' Exp ')'            { Map $3 $5 defaultAnnotations }
    | slice '(' Exp ',' Exp ',' Exp ')'  { Slice $3 $5 $7 defaultAnnotations }
    | filter '(' Exp ',' Exp ')'         { Filter $3 $5 defaultAnnotations }

    -- Application
    | var Exp   %prec APP                          { App (Ident $1 defaultAnnotations) $2 defaultAnnotations }
    | '(' Exp ')' Exp %prec APP                    { App $2 $4 defaultAnnotations }
    | Exp '(' Exp ')' %prec APP                    { App $1 $3 defaultAnnotations }
    | Exp '(' Exp ',' Exps ')' %prec APP           { MultiApp $1 ([$3] ++ $5) defaultAnnotations }

Exps : Exp { [$1] }
     | Exp ',' Exps { [$1] ++ $3 }

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
