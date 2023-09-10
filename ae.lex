%{
#include <stdio.h>
#include <string.h>
#include "ae.h"
#include "ae.tab.h"

#define FOR_ESCAPED_CHARACTER_DO(DO)                                                                                                        \
  DO('a',  '\a')                                                                                                                            \
  DO('b',  '\b')                                                                                                                            \
  DO('e',  '\e')                                                                                                                            \
  DO('f',  '\f')                                                                                                                            \
  DO('n',  '\n')                                                                                                                            \
  DO('r',  '\r')                                                                                                                            \
  DO('t',  '\t')                                                                                                                            \
  DO('v',  '\v')                                                                                                                            \
  DO('\\', '\\')                                                                                                                            \
  DO('\'', '\'')                                                                                                                            \
  DO('\"', '\"')                                                                                                                            \
  DO('\?', '\?')                                                                                                                         

#define TOKENIZE(x, ae_type) return tokenize(#x, x, ae_type);
  
  enum yytokentype tokenize(
    const char * const name,
    enum yytokentype x,
    ae_type_t ae_type) {
    ae_object_init(&yylval);
    yylval.type  = ae_type;
    yylval.c_str = strdup(yytext);

    //// printf("Tokenize got %s.\n", ae_object_str(&yylval));

    switch (yylval.type) {
    case AE_STRING:
      // printf("Tokenized an AE_STRING.\n");
      // yylval.string_value = strdup(yytext);
      yylval.string_value = malloc(strlen(yytext) - 1);
      strncpy(yylval.string_value, yytext + 1, strlen(yytext) - 2);
      printf("Parsed string [%s].\n", yylval.string_value);
      break;
    case AE_CHAR:
      // printf("\nTokenized an AE_CHAR.\n");
      // printf("Str [%s].\n", yytext);
      // printf("Len %d.\n", strlen(yytext));

      yylval.char_value = 0;

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
#define escaped_char_case(chr, replacement)                                                                                                 \
          case chr:                                                                                                                         \
            yylval.char_value = replacement;                                                                                           \
            break;
          FOR_ESCAPED_CHARACTER_DO(escaped_char_case);
        default:
          printf("Unrecognized escape sequence in [%s]!\n", yytext);
          break;
        }
      }
      else {
        // printf("Plain character.\n");
        yylval.char_value = tmp[0];
      }
      
      // printf("Final char '%c'.\n", yylval.char_value);

      free(tmp);
      
      break;
    case AE_INTEGER:
      // printf("Tokenized an AE_INTEGER.\n");
      yylval.int_value = atoi(yytext);
      // printf("Parsed integer %d.\n", yylval.int_value);
      break;
    case AE_FLOAT:
      // printf("Tokenized an AE_FLOAT.\n");
      yylval.float_value = strtod(yytext, 0);
      // printf("Parsed float %lf.\n", yylval.float_value);
      break;
    case AE_RATIONAL:
      // printf("Tokenized an AE_RATIONAL.\n");
      int slash_pos = 0;
      for (; yytext[slash_pos] != '/'; ++slash_pos);
      // printf("Slash pos %d.\n", slash_pos);

      char * tmp2 = malloc(slash_pos + 1);
      strncpy(tmp2, yytext, slash_pos);
      // printf("Copied [%s].\n", tmp2);
      yylval.rational_value.numerator = atoi(tmp2);
      // printf("Parsed [%d].\n", yylval.rational_value.numerator);
      free(tmp2);

      tmp2 = malloc(strlen(yytext) - slash_pos);
      // printf("Alloced %d.\n", strlen(yytext) - slash_pos);
      strncpy(tmp2, yytext + slash_pos + 1, strlen(yytext) - slash_pos - 1);
      // printf("Copied [%s].\n", tmp2);
      yylval.rational_value.denominator = atoi(tmp2);
      // printf("Parsed [%d].\n", yylval.rational_value.denominator);
      free(tmp2);
      
      break;
    case AE_SYMBOL:
      yylval.string_value = strdup(yytext);
      break;
    case AE_QUOTE:
      break;
    case AE_PAREN:
      break;
    case AE_LIST:
      break;
    default:
      printf("Tokenized something unrecognizable!\n");
      break;
    }

    return x;
  }
%}

%%
nil                                                             TOKENIZE(LIST,     AE_LIST    );
\'                                                              TOKENIZE(QUOTE,    AE_QUOTE   );
\(                                                              TOKENIZE(LPAREN,   AE_PAREN   );
\)                                                              TOKENIZE(RPAREN,   AE_PAREN   );                                                                
\"((\\\")|([^\"]))*\"                                           TOKENIZE(STRING,   AE_STRING  );
'[^']'       |
'\\.'        | 
\?\\\\.      |
\?\\.                                                           TOKENIZE(CHAR,     AE_CHAR    );
[-+]?[0-9]+                                                     TOKENIZE(INTEGER,  AE_INTEGER );
[-+]?[0-9]+\.[0-9]* |
[-+]?[0-9]*\.[0-9]+                                             TOKENIZE(FLOAT,    AE_FLOAT   );
[-+]?[0-9]+\/[0-9]+                                             TOKENIZE(RATIONAL, AE_RATIONAL);
[\+\-\/\*]                                                      TOKENIZE(MATHOP,   AE_SYMBOL  );
([1-9][0-9]+)?[\+\-\/\*]                                        TOKENIZE(INCROP,   AE_SYMBOL  );
!?=|(>=?)|(<=?)                                                 TOKENIZE(COMPARE,  AE_SYMBOL  );
(\-+)?([a-zA-Z][a-zA-Z0-9]*)(((\-+)|\/+)([a-zA-Z0-9]+))*[\?\!]? TOKENIZE(SYMBOL,   AE_SYMBOL  );
[\f\n\t\v\ ]+  ;

%%

