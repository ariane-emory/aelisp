%{
#include <stdio.h>
#include <string.h>
#include "ae.h"
#include "ae.tab.h"

#define LEX(x, ae_type) return lex(#x, x, ae_type);

  enum yytokentype lex(
    const char * const name,
    enum yytokentype x,
    ae_type_t ae_type) {
    ae_object_init(&yylval);
    yylval.type  = ae_type;
    yylval.c_str = strdup(yytext);

    //// printf("Lex got %s.\n", ae_object_str(&yylval));

    switch (yylval.type) {
    case AE_STRING:
      //printf("Lex a AE_STRING.\n");
      yylval.data.string_value = strdup(yytext);
      break;
    case AE_CHAR:
      //printf("Lex a AE_CHAR.\n");
      break;
    case AE_INTEGER:
      //printf("Lex a AE_INTEGER.\n");
      break;
    case AE_FLOAT:
      //printf("Lex a AE_FLOAT.\n");
      break;
    case AE_RATIONAL:
      //printf("Lex a AE_RATIONAL.\n");
      break;
    case AE_LIST:
      //printf("Lex a AE_LIST.\n");
      break;
    case AE_SYMBOL:
      //printf("Lex a AE_SYMBOL.\n");
      break;
    case AE_QUOTE:
      //printf("Lex a AE_QUOTE.\n");
      break;
    case AE_PAREN:
      //printf("Lex a AE_PAREN.\n");
      break;
    default:
      //printf("Lex something unrecognizable.\n");
      break;
    }

    return x;
  }
%}

%%
\'                                                              LEX(QUOTE,    AE_QUOTE   );
\(                                                              LEX(LPAR,     AE_PAREN   );
\)                                                              LEX(RPAR,     AE_PAREN   );                                                                
\"((\\\")|([^\"]))*\"                                           LEX(STRING,   AE_STRING  );
'[^']'              |
'\\[nt\ \\]\'       | 
\?\\\\[nt\ \\]      |
\?\\[^nt\ \\]                                                   LEX(CHAR,     AE_CHAR    );
[-+]?[0-9]+                                                     LEX(INTEGER,  AE_INTEGER );
[-+]?[0-9]+\.[0-9]* |
[-+]?[0-9]*\.[0-9]+                                             LEX(FLOAT,    AE_FLOAT   );
[-+]?[0-9]+\/[0-9]+                                             LEX(RATIONAL, AE_RATIONAL);
[\+\-\/\*]                                                      LEX(MATHOP,   AE_SYMBOL  );
([1-9][0-9]+)?[\+\-\/\*]                                        LEX(INCROP,   AE_SYMBOL  );
!?=|(>=?)|(<=?)                                                 LEX(COMPARE,  AE_SYMBOL  );
(\-+)?([a-zA-Z][a-zA-Z0-9]*)(((\-+)|\/+)([a-zA-Z0-9]+))*[\?\!]? LEX(SYMBOL,   AE_SYMBOL  );
[\n\t\ ]+  ;
  
%%

