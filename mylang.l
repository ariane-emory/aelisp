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
    // printf("Lex got %s.\n", name);

#ifdef    USE_MYLANG_OBJECT
    mylang_object_init(&yylval);
    yylval.type  = mylang_type;
    yylval.c_str = strdup(yytext);
    printf("Lex got %s.\n", mylang_object_str(&yylval));
#else  // USE_MYLANG_OBJECT
    yylval = strdup(yytext);
#endif // USE_MYLANG_OBJECT

    return x;
  }
%}

/* lex mylang.l && gcc-13 lex.yy.c -o mylang -ll && ./mylang  */
/* \{                                                              PP(LBRC,     ML_INVALID ); */
/* \}                                                              PP(RBRC,     ML_INVALID ); */
/* \[                                                              PP(LSQR,     ML_INVALID ); */
/* \]                                                              PP(RSQR,     ML_INVALID ); */
/* \,                                                              PP(COMMA,    ML_INVALID ); */
/* \;                                                              PP(SEMI,     ML_INVALID ); */

%%
\'                                                              PP(QUOTE,    ML_QUOTE   );
\(                                                              PP(LPAR,     ML_PAREN   );
\)                                                              PP(RPAR,     ML_PAREN   );
                                                                
\"((\\\")|[^\"]s)*\"                                            PP(STRING,   ML_STRING  );
\?\\?[^\ \n\t]      |
\'(\\['\(\)])\'     |
\'([^'\ \n\t])\'                                                PP(CHAR,     ML_CHAR    );
[-+]?[0-9]+                                                     PP(INTEGER,  ML_INTEGER );
[-+]?[0-9]+\.[0-9]* |
[-+]?[0-9]*\.[0-9]+                                             PP(FLOAT,    ML_FLOAT   );
[-+]?[0-9]+\/[0-9]+                                             PP(RATIONAL, ML_RATIONAL);

[\+\-\/\*]                                                      PP(MATHOP,   ML_SYMBOL  );
([1-9][0-9]+)?[\+\-\/\*]                                        PP(INCROP,   ML_SYMBOL  );
!?=|(>=?)|(<=?)                                                 PP(COMPARE,  ML_SYMBOL  );
(\-+)?([a-zA-Z][a-zA-Z0-9]*)(((\-+)|\/+)([a-zA-Z0-9]+))*[\?\!]? PP(WORD,     ML_SYMBOL  );
[\n\t\ ]+  ;
%

