grammar Tosa;

@header {
    package io.github.hexadecimaaal.tosa;
}

statement : definition | expression ;

definition : pattern ':=' expression ;

pattern
    : IDENTIFIER
    | '[' pattern * ']'
    | '[' pattern ('|' pattern)+ ']'
    | '_'
    ;

expression
    : '\\' pattern '.' expression
    | expression expression
    ;


IDENTIFIER : [a-zA-Z][a-zA-Z0-9_]* ;
WHITESPACE : (' ' | '\t') -> skip ;
