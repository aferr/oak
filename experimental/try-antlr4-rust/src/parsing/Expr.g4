grammar Expr; 
prog:    (expr NEWLINE)* ;
expr:     expr opr=('+'|'-') expr #op
    |    INT #literal
    |    '(' expr ')' #parens
    ;
NEWLINE : [\r\n]+ ;
INT     : [0-9]+ ;
