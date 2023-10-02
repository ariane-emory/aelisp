#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <assert.h>

#include "ae_write.h"
#include "ae_env.h"
#include "ae_alist.h"
#include "ae_list.h"
#include "ae_util.h"

static int ae_fwrite_internal(const ae_obj_t * const this);

////////////////////////////////////////////////////////////////////////////////////////////////////
// helpers
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
// data
////////////////////////////////////////////////////////////////////////////////////////////////////

static FILE * fwrite_stream   = NULL;
static int    fwrite_counter  = 0;
static bool   fwrite_quoting  = false;

////////////////////////////////////////////////////////////////////////////////////////////////////
// macros
////////////////////////////////////////////////////////////////////////////////////////////////////

#define COUNTED_FPUTC(c, stream)     fwrite_counter += (fputc((c), (stream)) == EOF ? 0 : 1)
#define COUNTED_FPUTS(s, stream)     fwrite_counter += (fputs((s), (stream)))
#define COUNTED_FPRINTF(stream, ...) fwrite_counter += (fprintf((stream), __VA_ARGS__))

// free this string when you're done with it:
#define DEF_S_METHOD(name)                                                                         \
char * ae_s ## name(const ae_obj_t * const this) {                                                 \
  char * buff;                                                                                     \
  size_t size;                                                                                     \
  FILE * stream = open_memstream(&buff, &size);                                                    \
                                                                                                   \
  ae_f ## name(this, stream);                                                                      \
  fclose(stream);                                                                                  \
  return buff;                                                                                     \
}

#define DEF_F_METHOD(name, quotes)                                                                 \
int ae_f ## name(const ae_obj_t * const this, FILE * stream) {                                     \
  assert(! NULLP((this)));                                                                       \
  FWRITE_RESET(stream, (quotes));                                                                  \
  return ae_fwrite_internal(this);                                                                 \
}

#define FWRITE_RESET(stream, quotes)                                                               \
  fwrite_quoting = (quotes);                                                                       \
  fwrite_counter = 0;                                                                              \
  fwrite_stream  = (stream)

#define DEF_STDOUT_METHOD(name) \
  int ae_ ## name (const ae_obj_t * const this) { return ae_f ## name(this, stdout); }

////////////////////////////////////////////////////////////////////////////////////////////////////
//  short methods
////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_STDOUT_METHOD(princ);
DEF_F_METHOD(princ, false);
DEF_S_METHOD(princ);

DEF_STDOUT_METHOD(write);
DEF_F_METHOD(write, true);
DEF_S_METHOD(write);

DEF_STDOUT_METHOD(put);
DEF_S_METHOD(put);

DEF_STDOUT_METHOD(print);
DEF_S_METHOD(print);

////////////////////////////////////////////////////////////////////////////////////////////////////
// _fprint
////////////////////////////////////////////////////////////////////////////////////////////////////

int ae_fprint(const ae_obj_t * const this, FILE * stream) {
  FNL;
  return 1 + ae_fwrite(this, stream);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// obj's fput_words
////////////////////////////////////////////////////////////////////////////////////////////////////

DEF_S_METHOD(put_words);
int ae_put_words(const ae_obj_t * const this) { return ae_fput_words(this, stdout); }

int ae_fput_words(const ae_obj_t * const this, FILE * stream) {
  assert(! NULLP(this));

  int                         written = 0;
  const unsigned char * const start   = (unsigned char *)this;

  // This assumes the system is little-endian and renders the values as if they were big-endian.

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

////////////////////////////////////////////////////////////////////////////////////////////////////
// _fputs
////////////////////////////////////////////////////////////////////////////////////////////////////

int ae_fput(const ae_obj_t * const this, FILE * stream) {
  assert(! NULLP(this));

  FWRITE_RESET(stream, false);

  switch (GET_TYPE(this)) {
  case AE_CORE:
  case AE_ENV:
  case AE_LAMBDA:
  case AE_MACRO:
  case AE_ERROR:
    return FPRINC (this, stream);
  default:
    break;
  }

  COUNTED_FPRINTF(stream, GET_TYPE_STR(this));
  COUNTED_FPRINTF(stream, "< ");
  COUNTED_FPRINTF(stream, "%018p, ", this);
  
  switch (GET_TYPE(this)) {
  case AE_CONS:
    COUNTED_FPRINTF(stream,
                    "%018p, %018p, %d",
                    CAR(this),
                    CDR(this),
                    LENGTH(this));
    break;
  case AE_CHAR:
    COUNTED_FPUTC('\'', fwrite_stream);
    fwrite_counter = fwrite_counter + FPRINC (this, stream); // this will reset, hence the addition.
    COUNTED_FPUTC('\'', fwrite_stream);
    break;
  case AE_STRING:
    COUNTED_FPUTC('\"', fwrite_stream);
    fwrite_counter = fwrite_counter + FPRINC (this, stream); // this will reset, hence the addition.
    COUNTED_FPUTC('\"', fwrite_stream);
    break;
  default:
    fwrite_counter = fwrite_counter + FPRINC (this, stream); // this will reset, hence the addition.
  }

  COUNTED_FPRINTF(stream, " >");

  return fwrite_counter;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// the main method
////////////////////////////////////////////////////////////////////////////////////////////////////

#define escaped_char_case_char(displayed, unescaped)                                               \
  case unescaped:                                                                                  \
    tmp[0] = '\\';                                                                                 \
    tmp[1] = displayed;                                                                            \
    break;

#define escaped_char_case_str(in, out)                                                             \
  case in:                                                                                         \
    COUNTED_FPUTC('~', stream);                                                                    \
    COUNTED_FPUTC(out, stream);                                                                    \
    break;

static int ae_fwrite_internal(const ae_obj_t * const this) {
  FILE * stream = fwrite_stream;

  switch (GET_TYPE(this)) {
  case AE_ERROR:
    COUNTED_FPRINTF(fwrite_stream, "%s< %018p, %018p, %s >", GET_TYPE_STR(this), this, ERR_OBJ(this), ERR_MSG(this));
    break;
  case AE_CORE:
    if (SPECIALP(this))
      COUNTED_FPRINTF(fwrite_stream, "%s< %018p, %s* >", GET_TYPE_STR(this), this, CORE_NAME(this));
    else
      COUNTED_FPRINTF(fwrite_stream, "%s< %018p, %s >", GET_TYPE_STR(this), this, CORE_NAME(this));
    break;
  case AE_ENV:
#ifdef AE_OBJ_DEBUG_DATA
    if (! NILP(AGET(DEBUG_DATA(this), SYM(":fun")))) {
      if (COREP((AGET(DEBUG_DATA(this), SYM(":fun"))))) {
        char * fun_name = CORE_NAME(AGET(DEBUG_DATA(this), SYM(":fun")));
        
        if (NILP(ENV_PARENT(this)))
          COUNTED_FPRINTF(fwrite_stream, "%s< %s, %018p → nil, %s >", GET_TYPE_STR(this), fun_name, this );
        else
          COUNTED_FPRINTF(fwrite_stream, "%s< %s, %018p → %018p >", GET_TYPE_STR(this), fun_name, this, ENV_PARENT(this));
      }
      else if (LAMBDAP((AGET(DEBUG_DATA(this), SYM(":fun"))))) {
        char * fun_name = SYM_VAL(AGET(DEBUG_DATA(AGET(DEBUG_DATA(this), SYM(":fun"))), SYM(":last-bound-to")));
        
        if (NILP(ENV_PARENT(this))) 
          COUNTED_FPRINTF(fwrite_stream, "%s< λ %s, %018p → nil >",
                          GET_TYPE_STR(this),
                          fun_name,
                          this);
        else 
          COUNTED_FPRINTF(fwrite_stream, "%s< λ %s, %018p → %018p >",
                          GET_TYPE_STR(this),
                          fun_name,
                          this,
                          ENV_PARENT(this));
      }
    }    
    else
#endif
      if (true) {
        goto print_env_without_name;
      }
    else {
    print_env_without_name:
    if (NILP(ENV_PARENT(this))) {
      COUNTED_FPRINTF(fwrite_stream, "%s< %018p → nil >", GET_TYPE_STR(this), this);
    }
    else {
      COUNTED_FPRINTF(fwrite_stream, "%s< %018p → %018p >", GET_TYPE_STR(this), this, ENV_PARENT(this));
    }
  }
  fwrite_counter--;
  break;
  case AE_LAMBDA:
  case AE_MACRO:
#ifdef AE_OBJ_DEBUG_DATA
    if (AHAS(DEBUG_DATA(this), SYM(":last-bound-to"))) {
      COUNTED_FPRINTF(fwrite_stream, "%s< %s, %018p, ",
                      GET_TYPE_STR(this),
                      SYM_VAL(AGET(DEBUG_DATA(this), SYM(":last-bound-to"))),
                      this);
      ae_fwrite_internal(FUN_PARAMS(this));
      COUNTED_FPRINTF(fwrite_stream,">");
    }
    else 
#endif
    {
      COUNTED_FPRINTF(fwrite_stream, "%s< %018p, ", GET_TYPE_STR(this), this);
      ae_fwrite_internal(FUN_PARAMS(this));
      COUNTED_FPRINTF(fwrite_stream,">");      
    }
    break;
  case AE_CONS:
    FLPAR;
    fwrite_quoting = true;

    FOR_EACH_CONST(elem, this) {
      ae_fwrite_internal(elem);
      fflush(fwrite_stream);

      if (! TAILP(CDR(position))) {
        COUNTED_FPRINTF(fwrite_stream, " . ");
        ae_fwrite_internal(CDR(position));
      } else if (! NILP(CDR(position))) {
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

      COUNTED_FPUTS(SYM_VAL(this), fwrite_stream);

      if (fwrite_quoting)
        COUNTED_FPUTC('"', fwrite_stream);
    }
    break;
  case AE_INTEGER:
    COUNTED_FPRINTF(fwrite_stream, "%d", INT_VAL(this));
    break;
  case AE_RATIONAL:
    COUNTED_FPRINTF(fwrite_stream, "%d/%d", NUMER_VAL(this) , DENOM_VAL(this));
    break;
  case AE_FLOAT:
    COUNTED_FPRINTF(fwrite_stream, "%g", FLOAT_VAL(this));
    break;
  case AE_CHAR:
  {
    char tmp[3] = { 0 };

    switch (this->char_val) {
      FOR_EACH_ESCAPED_CHARACTER(escaped_char_case_char);
    default:
      tmp[0] = CHAR_VAL(this);
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
