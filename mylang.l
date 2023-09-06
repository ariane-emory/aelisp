%{
#include <stdio.h>
#include <string.h>
#include "mylang-data.h"
#include "y.tab.h"

#define PP(x, mylang_type) return pp(#x, x, mylang_type);

  enum yytokentype pp(
    const char * const name,
    enum yytokentype x,
    mylang_type_t mylang_type) {
    printf("Lex got %s.\n", name);
#ifdef    USE_MYLANG_OBJECT
    mylang_object_init(&yylval);
#else  // USE_MYLANG_OBJECT
    yylval = strdup(yytext);
#endif // USE_MYLANG_OBJECT
    return x;
  }
%}

/* lex mylang.l && gcc-13 lex.yy.c -o mylang -ll && ./mylang  */

%%
\{                                  PP(LBRC, ML_INVALID);
\}                                  PP(RBRC, ML_INVALID);
\[                                  PP(LSQR, ML_INVALID);
\]                                  PP(RSQR, ML_INVALID);
\,                                  PP(COMMA, ML_INVALID);
\;                                  PP(SEMI, ML_INVALID);
\'                                  PP(QUOTE, ML_INVALID);

\(                                  PP(LPAR, ML_INVALID);
\)                                  PP(RPAR, ML_INVALID);
\"((\\\")|[^\"]s)*\"                PP(STRING, ML_INVALID);
\?\\?[^\ \n\t]      |
\'(\\['\(\)])\'     |
\'([^'\ \n\t])\'                    PP(CHAR, ML_INVALID);                                    
[-+]?[0-9]+                         PP(INTEGER, ML_INVALID);
[-+]?[0-9]+\.[0-9]* |
[-+]?[0-9]*\.[0-9]+                 PP(FLOAT, ML_INVALID);
[-+]?[0-9]+\/[0-9]+                 PP(RATIONAL, ML_INVALID);

[\+\-\/\*]                          PP(MATHOP, ML_INVALID);
([1-9][0-9]+)?[\+\-\/\*]            PP(INCROP, ML_INVALID);
!?=|(>=?)|(<=?)                     PP(COMPARE, ML_INVALID);
(\-+)?([a-zA-Z][a-zA-Z0-9]*)(((\-+)|\/+)([a-zA-Z0-9]+))*[\?\!]? PP(WORD, ML_INVALID);
[\n\t\ ]+  ;
%

