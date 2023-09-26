#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <assert.h>

#include "ae_write.h"
#include "ae_env.h"
#include "ae_list.h"
#include "ae_util.h"

static int ae_fwrite_internal(const ae_obj_t * const this);

////////////////////////////////////////////////////////////////////////////////////////////////////
// macros
////////////////////////////////////////////////////////////////////////////////////////////////////

#define MEMSTREAM(buff, stream)                                                                    \
  char * buff;                                                                                     \
  size_t size;                                                                                     \
  FILE * stream = open_memstream(&buff, &size);

// free this string when you're done with it:
#define DEF_S_METHOD(name)                                                                         \
char * ae_s ## name(const ae_obj_t * const this) {                                                 \
  MEMSTREAM(buff, stream);                                                                         \
  ae_f ## name(this, stream);                                                                      \
  fclose(stream);                                                                                  \
  return buff;                                                                                     \
}

#define DEF_F_METHOD(name, quotes, calls)                                                          \
int ae_f ## name(const ae_obj_t * const this, FILE * stream_) {                                    \
  ASSERT_NOT_NULLP(this);                                                                          \
                                                                                                   \
  fwrite_quoting = quotes;                                                                         \
  fwrite_counter = 0;                                                                              \
  fwrite_stream  = stream_;                                                                        \
                                                                                                   \
  return calls(this);                                                                              \
}


#define COUNTED_FPUTC(c, stream)     fwrite_counter += (fputc((c), (stream)) == EOF ? 0 : 1)
#define COUNTED_FPUTS(s, stream)     fwrite_counter += (fputs((s), (stream)))
#define COUNTED_FPRINTF(stream, ...) fwrite_counter += (fprintf((stream), __VA_ARGS__))

////////////////////////////////////////////////////////////////////////////////////////////////////
// data
////////////////////////////////////////////////////////////////////////////////////////////////////

static FILE * fwrite_stream   = NULL;
static int    fwrite_counter  = 0;
static bool   fwrite_quoting  = false;

////////////////////////////////////////////////////////////////////////////////////////////////////
// princ helpers
////////////////////////////////////////////////////////////////////////////////////////////////////

#define FNL      COUNTED_FPUTC('\n', stream)
#define FSPC     COUNTED_FPUTC(' ',  stream)
#define FLPAR    COUNTED_FPUTC('(',  stream)
#define FRPAR    COUNTED_FPUTC(')',  stream)
#define FLSQR    COUNTED_FPUTC('[',  stream)
#define FRSQR    COUNTED_FPUTC(']',  stream)
#define FDQUO    COUNTED_FPUTC('"',  stream)
#define FSQUO    COUNTED_FPUTC('\'', stream)

////////////////////////////////////////////////////////////////////////////////////////////////////
// obj's fput / put
////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_S_METHOD(put);

int ae_fput(const ae_obj_t * const this, FILE * stream) {
  ASSERT_NOT_NULLP(this);

  int written = fprintf(stream, "[ ", this);

  written    += fprintf(stream, "%s ", TYPE_STR(this));

  while (written++ <= 13) FSPC;

  switch (GET_TYPE(this)) {
  case AE_CONS:
    /* if (NOT_TAILP(CDR(this))) */
    /*   written  += fprintf(stream, "%018p %018p %-18s", CAR(this), CDR(this), "(pair)"); */
    /* else */
    written  += fprintf(stream, "%018p %018p %-9s % 8d",
                        CAR(this),
                        CDR(this),
                        (PROPER_LISTP(this) ? "" : ""), // "improper"),
                        LENGTH(this));
    break;
  case AE_LAMBDA:
    written  += fprintf(stream, "%018p %018p %018p", this->params, this->body, this->env);
    break;
  case AE_ENV:
    written  += fprintf(stream, "%018p %018p %018p", this->parent, this->symbols, this->values);
    break;
  case AE_CORE:
    written  += fprintf(stream, "% -18s %-18s %018p", CORE_NAME(this), (SPECIALP(this) ? "special" : "-"), CORE_FUN(this));
    break;
  default:
    written  += FPRINC(this, stream);
  }

  while (written++ <= 70) FSPC;

  FSPC;
  FRSQR;

  written    += 2;

  return written;
}

int ae_put(const ae_obj_t * const this) {
  return FPUT(this, stdout);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// obj's fput_words / put_words
////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_S_METHOD(put_words);

int ae_fput_words(const ae_obj_t * const this, FILE * stream) {
  ASSERT_NOT_NULLP(this);

  int                         written = 0;
  const unsigned char * const start   = (unsigned char *)this;

  // This assumes the system is little-endian and renders the values as big-endian.

  for (unsigned int ix = 0; ix < sizeof(*this); ix++) {
    if (ix % 8 == 0)
      written += fputs("0x", stream);

    written += fprintf(stream, "%02x", start[(7 - (ix % 8)) + (ix & ~0x7)]);

    if ((ix + 1) % 8 == 0) {
      written += fputs(" ", stream);
    }
  }

  return written;
}

int ae_put_words(const ae_obj_t * const this) {
  return ae_fput_words(this, stdout);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// obj's _princ methods
////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_F_METHOD(princ, false, ae_fwrite_internal);
DEF_S_METHOD(princ);

int ae_princ(const ae_obj_t * const this) {
  return FPRINC(this, stdout);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// obj's _write methods
////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_F_METHOD(write, true, ae_fwrite_internal);
DEF_S_METHOD(write);

int ae_write(const ae_obj_t * const this) {
  return FWRITE(this, stdout);
}

static int ae_fwrite_internal(const ae_obj_t * const this) {
  FILE * stream = fwrite_stream;

  switch (GET_TYPE(this)) {
  case AE_ENV:
    if (NILP(ENV_PARENT(this)))
      COUNTED_FPRINTF(fwrite_stream, "%s<%018p→nil>", TYPE_STR(this), this);
    else
      COUNTED_FPRINTF(fwrite_stream, "%s<%018p→%018p>", TYPE_STR(this), this, ENV_PARENT(this));
    break;
  case AE_CORE:
    if (SPECIALP(this))
      COUNTED_FPRINTF(fwrite_stream,
                      "%s<%s, %018p, special>",
                      TYPE_STR(this),
                      CORE_NAME(this),
                      CORE_FUN(this));
    else
      COUNTED_FPRINTF(fwrite_stream,
                      "%s<%s, %018p>",
                      TYPE_STR(this),
                      CORE_NAME(this),
                      CORE_FUN(this));
    break;
  case AE_LAMBDA:
  case AE_MACRO:
    COUNTED_FPRINTF(fwrite_stream,
                    "%s<%018p, %018p, ",
                    TYPE_STR(this),
                    FUN_ENV(this),
                    FUN_BODY(this));

    fwrite_counter += FWRITE(FUN_PARAMS(this), fwrite_stream);
    
    COUNTED_FPUTC('>', fwrite_stream);
    
    break;
  case AE_INF:
    COUNTED_FPUTS("∞", fwrite_stream);
    break;
  case AE_CONS:
    FLPAR;

    FOR_EACH_CONST(elem, this) {
      ae_fwrite_internal(elem);
      fflush(fwrite_stream);

      if (NOT_TAILP(CDR(position))) {
        COUNTED_FPRINTF(fwrite_stream, " . ");
        ae_fwrite_internal(CDR(position));
      } else if (NOT_NILP(CDR(position))) {
        FSPC;
      }
    }

    FRPAR;
    break;
  case AE_SYMBOL:
    COUNTED_FPUTS(SYM_VAL(this), fwrite_stream);
    break;
  case AE_STRING:
    if (STR_VAL(this) == NULL) {
      COUNTED_FPUTS("(null)", fwrite_stream);
    }
    else {
      if (fwrite_quoting)
        COUNTED_FPUTC('"', fwrite_stream);

      COUNTED_FPUTS(STR_VAL(this), fwrite_stream);

      if (fwrite_quoting)
        COUNTED_FPUTC('"', fwrite_stream);
    }
    break;
  case AE_INTEGER:
    COUNTED_FPRINTF(fwrite_stream, "%d", this->int_val);
    break;
  case AE_RATIONAL:
    COUNTED_FPRINTF(fwrite_stream, "%d/%d", this->numerator_val, this->denominator_val);
    break;
  case AE_FLOAT:
    COUNTED_FPRINTF(fwrite_stream, "%g", this->float_val);
    break;
  case AE_CHAR:
  {
    char tmp[3] = { 0 };

    switch (this->char_val) {

#define escaped_char_case(displayed, unescaped)                                                    \
      case unescaped:                                                                              \
        tmp[0] = '\\';                                                                             \
        tmp[1] = displayed;                                                                        \
        break;
      FOR_EACH_ESCAPED_CHARACTER(escaped_char_case);
#undef escaped_char_case

    default:
      tmp[0] = this->char_val;
    }

    if (fwrite_quoting)
      COUNTED_FPUTC('\'', fwrite_stream);

    COUNTED_FPUTS(tmp, fwrite_stream);

    if (fwrite_quoting)
      COUNTED_FPUTC('\'', fwrite_stream);

    break;
  }
  default:
    COUNTED_FPRINTF(fwrite_stream, "??");
  }

  fflush(fwrite_stream);

  return fwrite_counter;
}
