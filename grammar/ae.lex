%{
#include <stdio.h>
#include <string.h>

#include "ae_obj.h"

#define YYSTYPE ae_obj_t *

#include "ae.tab.h"

#define TOKENIZE(yy_tok_type, ae_type) return tokenize(#yy_tok_type, yy_tok_type, ae_type);
  
  enum yytokentype tokenize(
    const char * const name,
    enum yytokentype yy_tok_type,
    const ae_type_t ae_type) {
#ifdef AE_LOG_LEX
    printf("Lex grabbed '%s', a %s with yy_tok_type %d.\n", yytext, ae_type_str(ae_type), yy_tok_type);
    fflush(stdout);
#endif

    yylval = 0; 

    if (yy_tok_type == NILTOK) {
      yylval = NIL;
#ifdef AE_LOG_LEX
      printf("Returning the nil object, ");
      PUT(yylval);
      putchar('\n');
      fflush(stdout);
#endif
      yy_tok_type = SYMBOL;
      goto end;
    }
    
    switch (ae_type) {
    case AE_SYMBOL:
      yylval = INTERN(&symbols_list, yytext);
    case AE_INF:
    case AE_QUOTE:
    case AE_LPAREN:
    case AE_RPAREN:
      goto end;      
    }
        
    yylval = NEW(ae_type);
      
    switch (TYPE(yylval)) {
    case AE_CONS:
      break;
    case AE_INTEGER:
      INT_VAL(yylval) = atoi(yytext);
      break;
    case AE_FLOAT:
      FLOAT_VAL(yylval) = strtod(yytext, 0);
      break;
    case AE_RATIONAL:
      int slash_pos = 0;
      for (; yytext[slash_pos] != '/'; ++slash_pos);

      char * tmp2 = malloc(slash_pos + 1);
      strncpy(tmp2, yytext, slash_pos);
      NUMER_VAL(yylval) = atoi(tmp2);
      free(tmp2);

      tmp2 = malloc(strlen(yytext) - slash_pos);
      strncpy(tmp2, yytext + slash_pos + 1, strlen(yytext) - slash_pos - 1);
      DENOM_VAL(yylval) = atoi(tmp2);
      free(tmp2);
      
      break;
    case AE_STRING:
      yylval->str_val = malloc(strlen(yytext) - 1);
      strncpy(STR_VAL(yylval), yytext + 1, strlen(yytext) - 2);
      goto end;
    case AE_CHAR:
      char tmp[3];
      memset(&tmp[0], 0, 3);

      if (yytext[0] == '?')
        strncpy(tmp, yytext + 2, strlen(yytext) - 1);
      else
        strncpy(tmp, yytext + 1, strlen(yytext) - 2);

      if (tmp[0] == '\\') {
        switch(tmp[1]) {
#define escaped_char_case(chr, replacement)                                                        \
          case chr:                                                                                \
            yylval->char_val = replacement;                                                        \
            break;
          FOR_EACH_ESCAPED_CHARACTER(escaped_char_case);
        default:
          printf("Unrecognized escape sequence in [%s]!\n", yytext);
          break;
        }
      }
      else {
        yylval->char_val = tmp[0];
      }
      break;
    default:
      printf("Tokenized something unrecognizable!\n");
      break;
    }

  end:
#ifdef AE_LOG_LEX
    if (yylval) {
      // putchar('\n');
      printf("Tokenized ");
      PUT(yylval);
      printf(" as yy_tok_type %d.\n", yy_tok_type);
      fflush(stdout);
    }
#endif
    
    return yy_tok_type;
  }
%}

%%
∞                                                                      TOKENIZE(INF,      AE_INF     );
nil                 |
\([\f\n\t\v\ ]*\)                                                       TOKENIZE(NILTOK,   AE_SYMBOL  );
\'                                                                      TOKENIZE(QUOTE,    AE_QUOTE   );
\(                                                                      TOKENIZE(LPAREN,   AE_LPAREN  );
\)                                                                      TOKENIZE(RPAREN,   AE_RPAREN  );
\"((\\\")|([^\"]))*\"                                                   TOKENIZE(STRING,   AE_STRING  );
'[^']'              |
'\\.'               | 
\?\\\\.             |
\?\\.                                                                   TOKENIZE(CHAR,     AE_CHAR    );
[\+\-\/\*]          |
!?=|(>=?)|(<=?)     |
([\-+:&])?([a-zA-Z][a-zA-Z0-9\*]*)(((\-+)|\/+)([a-zA-Z0-9\*]+))*[\?\!]? TOKENIZE(SYMBOL,   AE_SYMBOL  );
([1-9][0-9]+)?[\+\-\/\*]                                                TOKENIZE(SYMBOL,   AE_SYMBOL  );
[-+]?[0-9]+                                                             TOKENIZE(INTEGER,  AE_INTEGER );
[-+]?[0-9]+\.[0-9]* | 
[-+]?[0-9]*\.[0-9]+                                                     TOKENIZE(FLOAT,    AE_FLOAT   );
[-+]?[0-9]+\/[0-9]+                                                     TOKENIZE(RATIONAL, AE_RATIONAL);
\;\;[^\n]*\n   ; /* comments */
[\f\n\t\v\ ]+  ; /* ignored wItespace */
%%

