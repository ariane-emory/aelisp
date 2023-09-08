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
      // printf("Lexed an AE_STRING.\n");
      // yylval.data.string_value = strdup(yytext);
      yylval.data.string_value = malloc(strlen(yytext) - 1);
      strncpy(yylval.data.string_value, yytext + 1, strlen(yytext) - 2);
      printf("Parsed string [%s].\n", yylval.data.string_value);
      break;
    case AE_CHAR:
      // printf("\nLexed an AE_CHAR.\n");
      // printf("Str [%s].\n", yytext);
      // printf("Len %d.\n", strlen(yytext));

      yylval.data.char_value = 0;

      char * tmp = 0;

      if (yytext[0] == '?') {
        tmp = malloc(strlen(yytext) - 1);
        strncpy(tmp, yytext + 2, strlen(yytext) - 1);
      }
      else {
        tmp = malloc(strlen(yytext) - 1);
        strncpy(tmp, yytext + 1, strlen(yytext) - 2);
      }

      // printf("Snipped [%s].\n", tmp);

      if (tmp[0] == '\\') {
        // printf("Escaped character.\n");
        switch(tmp[1]) {
        case 'n':
          yylval.data.char_value = '\n';
          break;
        case 't':
          yylval.data.char_value = '\t';
          break;
        case ' ':
          yylval.data.char_value = ' ';
          break;
        default:
          printf("Unrecognized escape sequence in [%s]!\n", yytext);
          break;
        }
      }
      else {
        // printf("Plain character.\n");
        yylval.data.char_value = tmp[0];
      }
      
      // printf("Final char '%c'.\n", yylval.data.char_value);

      free(tmp);
      
      break;
    case AE_INTEGER:
      // printf("Lexed an AE_INTEGER.\n");
      yylval.data.int_value = atoi(yytext);
      // printf("Parsed integer %d.\n", yylval.data.int_value);
      break;
    case AE_FLOAT:
      // printf("Lexed an AE_FLOAT.\n");
      yylval.data.float_value = strtod(yytext, 0);
      // printf("Parsed float %lf.\n", yylval.data.float_value);
      break;
    case AE_RATIONAL:
      // printf("Lexed an AE_RATIONAL.\n");
      int slash_pos = 0;
      for (; yytext[slash_pos] != '/'; ++slash_pos);
      // printf("Slash pos %d.\n", slash_pos);

      char * tmp2 = malloc(slash_pos + 1);
      strncpy(tmp2, yytext, slash_pos);
      // printf("Copied [%s].\n", tmp2);
      yylval.data.rational_value.numerator = atoi(tmp2);
      // printf("Parsed [%d].\n", yylval.data.rational_value.numerator);
      free(tmp2);

      tmp2 = malloc(strlen(yytext) - slash_pos);
      // printf("Alloced %d.\n", strlen(yytext) - slash_pos);
      strncpy(tmp2, yytext + slash_pos + 1, strlen(yytext) - slash_pos - 1);
      // printf("Copied [%s].\n", tmp2);
      yylval.data.rational_value.denominator = atoi(tmp2);
      // printf("Parsed [%d].\n", yylval.data.rational_value.denominator);
      free(tmp2);
      
      break;
    case AE_SYMBOL:
      yylval.data.string_value = strdup(yytext);
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

