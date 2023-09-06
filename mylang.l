%{
#include <stdio.h>
#include <string.h>
#include "mylang-data.h"
#include "y.tab.h"

#define LEX(x, mylang_type) return lex(#x, x, mylang_type);

  enum yytokentype lex(
    const char * const name,
    enum yytokentype x,
    mylang_type_t mylang_type) {
    mylang_object_init(&yylval);
    yylval.type  = mylang_type;
    yylval.c_str = strdup(yytext);

    printf("Lex got %s.\n", mylang_object_str(&yylval));

    switch (yylval.type) {
    case ML_STRING:
      printf("Lex a ML_STRING.\n");
      yylval.data.string_value = strdup(yytext);
      break;
    case ML_CHAR:
      printf("Lex a ML_CHAR.\n");
      break;
    case ML_INTEGER:
      printf("Lex a ML_INTEGER.\n");
      break;
    case ML_FLOAT:
      printf("Lex a ML_FLOAT.\n");
      break;
    case ML_RATIONAL:
      printf("Lex a ML_RATIONAL.\n");
      break;
    case ML_LIST:
      printf("Lex a ML_LIST.\n");
      break;
    case ML_SYMBOL:
      printf("Lex a ML_SYMBOL.\n");
      break;
    case ML_QUOTE:
      printf("Lex a ML_QUOTE.\n");
      break;
    case ML_PAREN:
      printf("Lex a ML_PAREN.\n");
      break;
    default:
      printf("Lex something unrecognizable.\n");
      break;
    }

    return x;
  }
%}

%%
\'                                                              LEX(QUOTE,    ML_QUOTE   );
\(                                                              LEX(LPAR,     ML_PAREN   );
\)                                                              LEX(RPAR,     ML_PAREN   );                                                                
\"((\\\")|([^\"]))*\"                                             LEX(STRING,   ML_STRING  );
\?\\?[^\ \n\t]      |
\'(\\['\(\)])\'     |
\'([^'\ \n\t])\'                                                LEX(CHAR,     ML_CHAR    );
[-+]?[0-9]+                                                     LEX(INTEGER,  ML_INTEGER );
[-+]?[0-9]+\.[0-9]* |
[-+]?[0-9]*\.[0-9]+                                             LEX(FLOAT,    ML_FLOAT   );
[-+]?[0-9]+\/[0-9]+                                             LEX(RATIONAL, ML_RATIONAL);
[\+\-\/\*]                                                      LEX(MATHOP,   ML_SYMBOL  );
([1-9][0-9]+)?[\+\-\/\*]                                        LEX(INCROP,   ML_SYMBOL  );
!?=|(>=?)|(<=?)                                                 LEX(COMPARE,  ML_SYMBOL  );
(\-+)?([a-zA-Z][a-zA-Z0-9]*)(((\-+)|\/+)([a-zA-Z0-9]+))*[\?\!]? LEX(WORD,     ML_SYMBOL  );
[\n\t\ ]+  ;
%

