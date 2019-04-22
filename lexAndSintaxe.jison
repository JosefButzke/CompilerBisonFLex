%lex

%%

\s+                                 {console.log('BRANCO'); /* ignorar */}
"char"                              {console.log('Token CHAR'); return 'CHAR';}
"("                                 {console.log('Token (' ); return '(';}
")"                                 {console.log('Token )' ); return ')';}
"*"                                 {console.log('Token *'); return '*';}
"+"                                 {console.log('Token +'); return '+';}
"-"                                 {console.log('Token -'); return '-';}
"/"                                 {console.log('Token /'); return '/';}
";"                                 {console.log('Token ;'); return ';';}
":"                                 {console.log('Token :'); return ':';}
"."                                 {console.log('Token .'); return '.';}
","                                 {console.log('Token ,'); return ',';}
"'"                                 {console.log('Token QUOTE'); return 'QUOTE';}
'"'                                 {console.log('Token DQUOTE'); return 'DQUOTE';}
"["                                 {console.log('Token ['); return '[';}
"]"                                 {console.log('Token ]'); return ']';}
"{"                                 {console.log('Token {'); return '{';}
"}"                                 {console.log('Token }'); return '}';}
"<="                                {console.log('Token LE'); return 'LE';}
">="                                {console.log('Token GE'); return 'GE';}
"=="                                {console.log('Token EQ'); return 'EQ';}
"!="                                {console.log('Token NE'); return 'NE';}
"<"                                 {console.log('Token <'); return '<';}
">"                                 {console.log('Token >'); return '>';}
"="                                 {console.log('Token ='); return '=';}
"||"                                {console.log('Token OR'); return 'OR';}
"!"                                 {console.log('Token NOT'); return 'NOT';}
"&&"                                {console.log('Token AND'); return 'AND';}
"else"                              {console.log('Token ELSE'); return 'ELSE';}
"if"                                {console.log('Token IF'); return 'IF';}
"while"                             {console.log('Token WHILE'); return 'WHILE';}
"for"                             {console.log('Token FOR'); return 'FOR';}
"switch"                            {console.log('Token SWITCH'); return 'SWITCH';}
"case"                              {console.log('Token CASE'); return 'CASE';}
"break"                             {console.log('Token BREAK'); return 'BREAK';}
"default"                           {console.log('Token DEFAULT'); return 'DEFAULT';}
"var"                               {console.log('Token VAR'); return 'VAR';}
"int"                               {return 'INT';}
"double"                            {console.log('Token DOUBLE'); return 'DOUBLE';}
"float"                             {console.log('Token FLOAT'); return 'FLOAT';}
"do"                          {console.log('Token DOWHILE'); return 'DO';}
"#"                                 {console.log('Token #'); return '#';}
"define"                            {console.log('Token DEFINE'); return 'DEFINE';}
[a-zA-Z][a-zA-Z0-9_]*               {console.log('Token IDF'); return 'IDF';}
[0-9]*\.[0-9]+([eE][+-][0-9]+)?     {console.log('Token F_LIT'); return 'F_LIT';}
[0-9]+                              {return 'INT_LIT';}
.                                   {console.log('Erro léxico: caractere [', yytext, '] não reconhecido.');}
<<EOF>>                             {return 'EOF';}

/lex

/* operator associations and precedence */

%left '<' '>' '=' NE LE GE
%left '+' '-' 
%left '*' '/'

%start corpo

%% /* language grammar */

corpo: statements EOF {console.log(1)}; 

statements : statements statement | statement;

statement
	: expression_statement
	| if_stmt
	| labeled_statement
	| repeat_statement
	;

expression_statement: expr ';'
	;

if_stmt:
    IF '(' conditional_expression ')' statement ELSE statement 
    | IF '(' conditional_expression ')' '{' statements '}' ELSE '{' statements '}'
    | SWITCH '(' IDF ')' '{' labeled_statements '}'
    ;

expr
    : expr '+' expr
    | expr '-' expr
    | expr '*' expr
    | expr '/' expr
    | '(' expr ')'
    | INT_LIT
    | F_LIT
    ;

conditional_expression: expr EQ expr | IDF '<' IDF | IDF '<' INT_LIT | IDF '=' INT_LIT;

variaveis
    : tipo IDF '=' expr';'
    | tipo IDF ';'
    ;
  
labeled_statements
    : labeled_statements labeled_statement | labeled_statement
    ;

repeat_statement
    : WHILE '(' conditional_expression ')' '{' statement '}'
    | FOR '(' IDF '=' INT_LIT ';' IDF '=' INT_LIT ';' IDF '+' '+' ';' ')' '{' statement '}'
    | DO '{' statement '}' WHILE '(' conditional_expression ')' ';'
    ;

labeled_statement
	: CASE INT_LIT ':' statement BREAK ';'
	;

tipo: INT | CHAR | FLOAT | DOUBLE | VAR;
