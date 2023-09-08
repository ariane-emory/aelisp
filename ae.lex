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
      //printf("Lexed an AE_STRING.\n");
      //yylval.data.string_value = strdup(yytext);
      yylval.data.string_value = malloc(strlen(yytext));
      char * begin = yytext + 1;
      int copy_len = strlen(begin) - 1;
      strncpy(yylval.data.string_value, begin, copy_len);
      printf("Parsed string [%s].\n", yylval.data.string_value);
      break;
    case AE_CHAR:
      //printf("Lexed an AE_CHAR.\n");
      break;
    case AE_INTEGER:
      //printf("Lexed an AE_INTEGER.\n");
      yylval.data.int_value = atoi(yytext);
      // printf("Parsed integer %d.\n", yylval.data.int_value);
      break;
    case AE_FLOAT:
      //printf("Lexed an AE_FLOAT.\n");
      yylval.data.float_value = strtod(yytext, 0);
      // printf("Parsed float %lf.\n", yylval.data.float_value);
      break;
    case AE_RATIONAL:
      //printf("Lexed an AE_RATIONAL.\n");
      break;
    case AE_SYMBOL:
      break;
    case AE_QUOTE:
      break;
    case AE_PAREN:
      break;
    default:
      printf("Lexed something unrecognizable!\n");
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

