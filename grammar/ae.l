h/* -*- mode: flex; c-basic-offset: 2; indent-tabs-mode: nil -*- */
%{
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include "common.h"
#include "env.h"
#include "eval.h"
#include "log.h"
#include "obj.h"
#include "write.h"
#include "free_list.h"

////////////////////////////////////////////////////////////////////////////////

#define YYSTYPE ae_obj_t *

#include "ae.tab.h"

////////////////////////////////////////////////////////////////////////////////

extern int yylineno;
extern ae_obj_t * program;

////////////////////////////////////////////////////////////////////////////////

void parse_line(const char * const line) {
      YY_BUFFER_STATE buffer = yy_scan_string(line);
      yylineno = 0;
  
      yyparse();

      yy_delete_buffer(buffer);
}

#include <stdio.h>
#include <ctype.h>

////////////////////////////////////////////////////////////////////////////////

long long int read_integer(const char *str) {
  // NOTE: This function will tolerate a comma as the first character of the
  // string or multiple commas in a row, but the token pattern it's used will
  // prohibit that.
  
  long long int result = 0;

  for (const char *p = str; *p; ++p) {
    if (isdigit((unsigned char)*p))
      result = result * 10 + (*p - '0');
    else if (*p != ',')
      break;
  }

  return result;
}

////////////////////////////////////////////////////////////////////////////////

#define TOKENIZE(yy_tok_type, ae_type) return tokenize(#yy_tok_type, yy_tok_type, ae_type);

////////////////////////////////////////////////////////////////////////////////

  enum yytokentype tokenize(
    const char * const name,
    enum yytokentype yy_tok_type,
    const ae_type_t ae_type) {
#ifdef AE_LOG_LEX
    if (ae_type == AE_INVALID) {
      printf("Lex grabbed '%s', with yy_tok_type %d.\n", yytext, yy_tok_type);
    }
    else {
      printf("Lex grabbed '%s', a %s with yy_tok_type %d.\n", yytext, TYPE_STR(ae_type), yy_tok_type);
    }
    fflush(stdout);
#endif

    yylval = 0;

    if (yy_tok_type == NILTOK) {
      yylval = NIL;
#ifdef AE_LOG_LEX
      printf("Returning nil, ");
      PUT(yylval);
      putchar('\n');
      fflush(stdout);
#endif
      yy_tok_type = SYMBOL;
      goto end;
    }

    if (ae_type == AE_INVALID)
      goto end;

    if (ae_type == AE_SYMBOL) {
      yylval = SYM(yytext);

      goto end;
    }

    /* if (yy_tok_type == MATH_SYMBOL) { */
    /*   yylval = SYM(yytext); */

    /*   goto end; */
    /* } */

    yylval = NEW(ae_type);

    switch (GET_TYPE(yylval)) {
    case AE_CONS:
      break;
    case AE_INTEGER:
      INT_VAL(yylval) = read_integer(yytext);
      break;
    case AE_FLOAT:
      FLOAT_VAL(yylval) = strtod(yytext, 0);
      break;
    case AE_RATIONAL:
      int slash_pos = 0;
      for (; yytext[slash_pos] != '/'; ++slash_pos);
      
      char * tmp2 = malloc(slash_pos + 1);
      strncpy(tmp2, yytext, slash_pos);
      NUMER_VAL(yylval) = read_integer(tmp2);
      free(tmp2);

      tmp2 = malloc(strlen(yytext) - slash_pos);
      strncpy(tmp2, yytext + slash_pos + 1, strlen(yytext) - slash_pos - 1);
      DENOM_VAL(yylval) = read_integer(tmp2);
      free(tmp2);

      break;
    case AE_STRING:
      // yylval->str_val = malloc(strlen(yytext) - 1);
      yylval->str_val = free_list_malloc(strlen(yytext) - 1);
      strncpy(STR_VAL(yylval), yytext + 1, strlen(yytext) - 2);
      STR_VAL(yylval)[strlen(yytext) - 2] = '\0';  // Null-terminate the string

      goto end;
    case AE_CHAR:
      char tmp[3];
      memset(&tmp[0], 0, 3);

      if (yytext[0] == '?')
        strncpy(tmp, yytext + 1, strlen(yytext) - 1);
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

////////////////////////////////////////////////////////////////////////////////

%}

%option never-interactive
%option yylineno
%x COMMENT

%%

"#|"   { BEGIN(COMMENT); }
<COMMENT>"|#" { BEGIN(INITIAL); }
<COMMENT>.|\n { /* consume everything inside multiline comments */ }
nil                                                                        |
\([\f\n\t\v\ ]*\)                                                          TOKENIZE(NILTOK,      AE_SYMBOL  );
\'                                                                         TOKENIZE(QUOTE,       AE_INVALID );
\`                                                                         TOKENIZE(BACKTICK,    AE_INVALID );
\,\@                                                                       TOKENIZE(COMMA_AT,    AE_INVALID );
\$                                                                         TOKENIZE(DOLLAR,      AE_INVALID );
\@                                                                         TOKENIZE(AT,          AE_INVALID );
\,                                                                         TOKENIZE(COMMA,       AE_INVALID );
\.                                                                         TOKENIZE(DOT,         AE_INVALID );
\(                                                                         TOKENIZE(LPAREN,      AE_INVALID );
\)                                                                         TOKENIZE(RPAREN,      AE_INVALID );
\"((\\\")|([^\"]))*\"                                                      TOKENIZE(STRING,      AE_STRING  );
'[^']'                                                                     |
'\\.'                                                                      |
\?\\.                                                                      |
\?.                                                                        TOKENIZE(CHAR,        AE_CHAR    );
\?\?\?                                                                           |
[\%\#]([a-zA-Z][a-zA-Z0-9]*)(((\-+)|\/+)([a-zA-Z0-9]+))*[\?\!\*\+]?              |
([1-9][0-9]*)?[\+\-\/\*]                                                         |
¬|λ\??|∧|∨|⊤|⊥|≤|≥|×|÷|Ø|∈|∉|≠|!|∃|∄|∀|≔|\||&|~|^|\?                             |
𝑎|𝑏|𝑐|𝑑|𝑒|𝑓|𝑚|𝑛|𝑜|𝑝|𝑞|𝑟|𝑠|𝑡|𝑢|𝑣|𝑤|𝑥|𝑦|𝑧                                          |
\<([a-zA-Z][a-zA-Z0-9\']*)(((\-+)|\/+)([a-zA-Z0-9\']+))*[\?\!\*\+]*\>            |
\*([a-zA-Z][a-zA-Z0-9\']*)(((\-+)|\/+)([a-zA-Z0-9\']+))*[\?\!\*\+]*\*            |
([\-+:&!])?([a-zA-Z][a-zA-Z0-9\'\*]*)(((\-+)|\/+)([a-zA-Z0-9\'\*]+))*[\?\!\*\+]* |
(_)|(=)|(==)|(!=)|(>=?)|(<=?)                                                    |
\<\<                                                                             |
\>\>                                                                             |
[\+\-\/\%\*]                                                               TOKENIZE(SYMBOL,      AE_SYMBOL  );
[-+]?[0-9]+(,[0-9]+)*                                                      TOKENIZE(INTEGER,     AE_INTEGER );
[-+]?[0-9]+\.[0-9]*                                                        |
[-+]?[0-9]*\.[0-9]+                                                        TOKENIZE(FLOAT,       AE_FLOAT   );
[-+]?[0-9]+(,[0-9]+)*\/[0-9]+(,[0-9]+)*                                    TOKENIZE(RATIONAL,    AE_RATIONAL);
\;[^\n]*\n                                                                 ; /* comments */
[\f\n\t\v\ ]+                                                              ; /* ignored whitespace */
. { 
    fprintf(stderr, "Unrecognized input at line %d: '%s'\n", yylineno, yytext); 
    read_error = true; 
    yyterminate(); 
}

%%
