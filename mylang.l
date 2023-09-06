%{
#include <stdio.h>
#include <string.h>
#include "mylang-data.h"
#include "y.tab.h"

#define PP(x) return pp(#x, x);

  enum yytokentype pp(const char * const name, enum yytokentype x) {
    printf("Lex got %s.\n", name);
    yylval = strdup(yytext);
    return x;
  }
%}

/* lex mylang.l && gcc-13 lex.yy.c -o mylang -ll && ./mylang  */

%%
\{                                  PP(LBRC);
\}                                  PP(RBRC);
\[                                  PP(LSQR);
\]                                  PP(RSQR);
\,                                  PP(COMMA);
\;                                  PP(SEMI);
\'                                  PP(QUOTE);

\(                                  PP(LPAR);
\)                                  PP(RPAR);
\"((\\\")|[^\"]s)*\"                PP(STRING);
\?\\?[^\ \n\t]      |
\'(\\['\(\)])\'     |
\'([^'\ \n\t])\'                    PP(CHAR);                                    
[-+]?[0-9]+                         PP(INTEGER);
[-+]?[0-9]+\.[0-9]* |
[-+]?[0-9]*\.[0-9]+                 PP(FLOAT);
[-+]?[0-9]+\/[0-9]+                 PP(RATIONAL);

[\+\-\/\*]                          PP(MATHOP);
([1-9][0-9]+)?[\+\-\/\*]            PP(INCROP);
!?=|(>=?)|(<=?)                     PP(COMPARE);
(\-+)?([a-zA-Z][a-zA-Z0-9]*)(((\-+)|\/+)([a-zA-Z0-9]+))*[\?\!]? PP(WORD);
[\n\t\ ]+  ;
%

