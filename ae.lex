%{
#include <stdio.h>
#include <string.h>

#include "ae_list.h"
#include "ae_obj.h"

#define YYSTYPE ae_obj_t
#include "ae.tab.h"

#define FOR_ESCAPED_CHARACTER_DO(DO)                                                                                                        \
  DO('a',  '\a')                                                                                                                            \
  DO('b',  '\b')                                                                                                                            \
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
    ae_obj_init(&yylval);
    yylval.type  = ae_type;
    yylval.c_str = strdup(yytext);

    switch (yylval.type) {
    case AE_STRING:
      yylval.str_value = malloc(strlen(yytext) - 1);
      strncpy(yylval.str_value, yytext + 1, strlen(yytext) - 2);
      break;
    case AE_SYMBOL:
      yylval.str_value = malloc(strlen(yytext) + 1);
      yylval.str_value = strdup(yytext);
      break;
    case AE_CHAR:
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

      if (tmp[0] == '\\') {
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
        yylval.char_value = tmp[0];
      }
      
      free(tmp);
      
      break;
    case AE_INTEGER:
      yylval.int_value = atoi(yytext);
      break;
    case AE_FLOAT:
      yylval.float_value = strtod(yytext, 0);
      break;
    case AE_RATIONAL:
      int slash_pos = 0;
      for (; yytext[slash_pos] != '/'; ++slash_pos);

      char * tmp2 = malloc(slash_pos + 1);
      strncpy(tmp2, yytext, slash_pos);
      yylval.numerator_value = atoi(tmp2);
      free(tmp2);

      tmp2 = malloc(strlen(yytext) - slash_pos);
      strncpy(tmp2, yytext + slash_pos + 1, strlen(yytext) - slash_pos - 1);
      yylval.denominator_value = atoi(tmp2);
      free(tmp2);
      
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

