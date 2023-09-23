#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <assert.h>

#include "ae_obj.h"
#include "ae_write.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// princ helpers
////////////////////////////////////////////////////////////////////////////////////////////////////

#define NL      fputc('\n', stream)
#define SPC     fputc(' ',  stream)
#define LPAR    fputc('(',  stream)
#define RPAR    fputc(')',  stream)
#define LSQR    fputc('[',  stream)
#define RSQR    fputc(']',  stream)
#define DQUO    fputc('"',  stream)
#define SQUO    fputc('\'', stream)
#define PR(...) (fprintf(stdout, __VA_ARGS__))

////////////////////////////////////////////////////////////////////////////////////////////////////
// obj's fput / put
////////////////////////////////////////////////////////////////////////////////////////////////////

int ae_fput(const ae_obj_t * const this, FILE * stream) {
  ASSERT_NOT_NULLP(this);
  
  int written = fprintf(stream, "[ ", this);

  written    += fprintf(stream, "%s ", TYPE_STR(GET_TYPE(this)));

  while (written++ <= 13) SPC;

  switch (GET_TYPE(this)) {
  case AE_CONS:
    written  += fprintf(stream, "%018p %018p %18d", CAR(this), CDR(this), LENGTH(this));
    break;
  case AE_ENV:
    written  += fprintf(stream, "%018p %018p %018p", this->parent, this->symbols, this->values);
    break;
  case AE_CORE_FUN:
    written  += fprintf(stream, "% -18s %-18s %018p", NAME_VAL(this), (SPECIAL_FUNP(this) ? "special" : "-"), FUN_VAL(this));
    break;
  default:
    written  += FPRINC(this, stream);
  }
  
  while (written++ <= 70) SPC;

  SPC;
  RSQR;

  written    += 2;
  
  return written;
}

int ae_put(const ae_obj_t * const this) {
  return FPUT(this, stdout);
}

#define MEMSTREAM(buff, stream)                                                                    \
  char * buff;                                                                                     \
  size_t size;                                                                                     \
  FILE * stream = open_memstream(&buff, &size);

char * ae_sput(const ae_obj_t * const this) {
  MEMSTREAM(buff, stream);

  ae_fput(this, stream);
  
  fclose(stream);

  return buff; // free this when you're done with it.
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// obj's fput_words / put_words
////////////////////////////////////////////////////////////////////////////////////////////////////

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

char * ae_sput_words(const ae_obj_t * const this) {
  MEMSTREAM(buff, stream);

  ae_fput_words(this, stream);
  
  fclose(stream);

  return buff; // free this when you're done with it.
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// obj's _princ methods
////////////////////////////////////////////////////////////////////////////////////////////////////

int ae_princ(const ae_obj_t * const this) {
  return FPRINC(this, stdout);
}

static FILE * fprinc_stream   = NULL;
static int    fprinc_counter  = 0;

#define COUNTED_FPUTC(c, stream)     fprinc_counter += (fputc((c), (stream)) == EOF ? 0 : 1)
#define COUNTED_FPUTS(s, stream)     fprinc_counter += (fputs((s), (stream)))
#define COUNTED_FPRINTF(stream, ...) fprinc_counter += (fprintf((stream), __VA_ARGS__))

static int ae_fprinc_internal(const ae_obj_t * const this) {
  switch (GET_TYPE(this)) {
  case AE_ENV:
    if (NILP(ENV_PARENT(this)))
      COUNTED_FPRINTF(fprinc_stream, "env<nil←%018p>", ENV_PARENT(this), this);
    else
      COUNTED_FPRINTF(fprinc_stream, "env<%018p←%018p>", ENV_PARENT(this), this);
    break;
  case AE_INF:
    COUNTED_FPUTS("∞", fprinc_stream);
    break;
  case AE_CONS:
    COUNTED_FPUTC('(', fprinc_stream);

    FOR_EACH_CONST(elem, this) {
      ae_fprinc_internal(elem);
      fflush(fprinc_stream);
        
      if (! NILP(CDR(position)))
        COUNTED_FPUTC(' ', fprinc_stream);
    }
      
    COUNTED_FPUTC(')', fprinc_stream);
    break;
  case AE_SYMBOL:
    COUNTED_FPUTS(SYM_VAL(this), fprinc_stream);
    break;
  case AE_STRING:
    if (STR_VAL(this) == NULL)
      COUNTED_FPUTS("(null)", fprinc_stream);
    else
      COUNTED_FPUTS(STR_VAL(this), fprinc_stream);
    break;
  case AE_INTEGER:
    COUNTED_FPRINTF(fprinc_stream, "%d", this->int_val);
    break;
  case AE_RATIONAL:
    COUNTED_FPRINTF(fprinc_stream, "%d/%d", this->numerator_val, this->denominator_val);
    break;
  case AE_FLOAT:
    COUNTED_FPRINTF(fprinc_stream, "%g", this->float_val);
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

    COUNTED_FPUTS(tmp, fprinc_stream);
    
    break;
  }
  default:
    COUNTED_FPRINTF(fprinc_stream, "??");
  }
  
  fflush(fprinc_stream);
  
  return fprinc_counter;
}

int ae_fprinc(const ae_obj_t * const this, FILE * stream_) {
  ASSERT_NOT_NULLP(this);

  fprinc_counter = 0;
  fprinc_stream  = stream_;

  return ae_fprinc_internal(this);
}

char * ae_sprinc(const ae_obj_t * const this) {
  MEMSTREAM(buff, stream_);

  ae_fprinc(this, stream_);
  fclose(fprinc_stream);

  return buff; // free this when you're done with it.
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// obj's _write methods
////////////////////////////////////////////////////////////////////////////////////////////////////

int ae_write(const ae_obj_t * const this) {
  return FWRITE(this, stdout);
}

char * ae_swrite(const ae_obj_t * const this) {
  MEMSTREAM(buff, stream_);

  ae_fwrite(this, stream_);
  fclose(fprinc_stream);

  return buff; // free this when you're done with it.
}

static int ae_fwrite_internal(const ae_obj_t * const this) {
  switch (GET_TYPE(this)) {
  case AE_ENV:
    if (NILP(ENV_PARENT(this)))
      COUNTED_FPRINTF(fprinc_stream, "env<nil←%018p>", ENV_PARENT(this), this);
    else
      COUNTED_FPRINTF(fprinc_stream, "env<%018p←%018p>", ENV_PARENT(this), this);
    break;
  case AE_INF:
    COUNTED_FPUTS("∞", fprinc_stream);
    break;
  case AE_CONS:
    COUNTED_FPUTC('(', fprinc_stream);

    FOR_EACH_CONST(elem, this) {
      ae_fwrite_internal(elem);
      fflush(fprinc_stream);
        
      if (! NILP(CDR(position)))
        COUNTED_FPUTC(' ', fprinc_stream);
    }
      
    COUNTED_FPUTC(')', fprinc_stream);
    break;
  case AE_SYMBOL:
    COUNTED_FPUTS(SYM_VAL(this), fprinc_stream);
    break;
  case AE_STRING:
    if (STR_VAL(this) == NULL) {
      COUNTED_FPUTS("(null)", fprinc_stream);
    }
    else {
      COUNTED_FPUTC('"', fprinc_stream);
      COUNTED_FPUTS(STR_VAL(this), fprinc_stream);
      COUNTED_FPUTC('"', fprinc_stream);
    }
    break;
  case AE_INTEGER:
    COUNTED_FPRINTF(fprinc_stream, "%d", this->int_val);
    break;
  case AE_RATIONAL:
    COUNTED_FPRINTF(fprinc_stream, "%d/%d", this->numerator_val, this->denominator_val);
    break;
  case AE_FLOAT:
    COUNTED_FPRINTF(fprinc_stream, "%g", this->float_val);
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

    COUNTED_FPUTC('\'', fprinc_stream);
    COUNTED_FPUTS(tmp, fprinc_stream);
    COUNTED_FPUTC('\'', fprinc_stream);
    
    break;
  }
  default:
    COUNTED_FPRINTF(fprinc_stream, "??");
  }
  
  fflush(fprinc_stream);
  
  return fprinc_counter;
}

int ae_fwrite(const ae_obj_t * const this, FILE * stream_) {
  ASSERT_NOT_NULLP(this);

  fprinc_counter = 0;
  fprinc_stream  = stream_;

  return ae_fwrite_internal(this);
}
