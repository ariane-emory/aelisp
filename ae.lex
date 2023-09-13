%{
#include <stdio.h>
#include <string.h>

#include "ae_obj.h"

#define YYSTYPE ae_obj_t
#include "ae.tab.h"

#define TOKENIZE(x, ae_type) return tokenize(#x, x, ae_type);
  
  enum yytokentype tokenize(
    const char * const name,
    enum yytokentype x,
    const ae_type_t ae_type) {
    memset(&yylval, 0, sizeof(yylval));
    
#ifdef NOISY_INIT
    printf("Initting yylval  <%p>\n", &yylval);
#endif

    ae_obj_init(&yylval, ae_type);

#ifdef NOISY_INIT
    printf("Initted yylval   <%p>\n\n", &yylval);
#endif
    
    switch (yylval.type) {
    case AE_STRING__:
      yylval.str_value = malloc(strlen(yytext) - 1);
      strncpy(yylval.str_value, yytext + 1, strlen(yytext) - 2);
      break;
    case AE_SYMBOL__:
      yylval.sym_value = malloc(strlen(yytext) + 1);
      yylval.sym_value = strdup(yytext);
      break;
    case AE_CHAR____:
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
    case AE_INTEGER_:
      yylval.int_value = atoi(yytext);
      break;
    case AE_FLOAT___:
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
    case AE_INF_____:
    case AE_QUOTE___:
    case AE_LPAREN__:
    case AE_RPAREN__:
    case AE_LIST____:
      break;
    default:
      printf("Tokenized something unrecognizable!\n");
      break;
    }

    return x;
  }
%}

%%
∞                                                                  TOKENIZE(INF,      AE_INF_____     );
nil                                                                 TOKENIZE(LIST,     AE_LIST____    );
\'                                                                  TOKENIZE(QUOTE,    AE_QUOTE___   );
\(                                                                  TOKENIZE(LPAREN,   AE_LPAREN__  );
\)                                                                  TOKENIZE(RPAREN,   AE_RPAREN__  );                                                                
\"((\\\")|([^\"]))*\"                                               TOKENIZE(STRING,   AE_STRING__  );
'[^']'       |
'\\.'        | 
\?\\\\.      |
\?\\.                                                               TOKENIZE(CHAR,     AE_CHAR____    );
[-+]?[0-9]+                                                         TOKENIZE(INTEGER,  AE_INTEGER_ );
[-+]?[0-9]+\.[0-9]* |
[-+]?[0-9]*\.[0-9]+                                                 TOKENIZE(FLOAT,    AE_FLOAT___   );
[-+]?[0-9]+\/[0-9]+                                                 TOKENIZE(RATIONAL, AE_RATIONAL);
[\+\-\/\*]                                                          TOKENIZE(MATHOP,   AE_SYMBOL__  );
([1-9][0-9]+)?[\+\-\/\*]                                            TOKENIZE(INCROP,   AE_SYMBOL__  );
!?=|(>=?)|(<=?)                                                     TOKENIZE(COMPARE,  AE_SYMBOL__  );
([\-+:&])?([a-zA-Z][a-zA-Z0-9]*)(((\-+)|\/+)([a-zA-Z0-9]+))*[\?\!]? TOKENIZE(SYMBOL,   AE_SYMBOL__  );
[\f\n\t\v\ ]+  ;

%%

