#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "core.h"
#include "obj.h"
#include "list.h"
#include "alist.h"
#include "plist.h"
#include "free_list.h"
#include "core.h"
#include "eval.h"
#include "write.h"
#include "env.h"
#include "util.h"
#include "generate_macro.h"

#define BEFORE_ACUTEST

#include "acutest.h"

#define free_list_size (1 << 16)

static char mem[free_list_size] = { 0 };

////////////////////////////////////////////////////////////////////////////////////////////////////
// Macros
////////////////////////////////////////////////////////////////////////////////////////////////////

#define T                         TEST_CHECK
#define TM                        TEST_MSG
#define SPC                       (putchar(' '))
#define NL                        (putchar('\n'))
#define FF                        (fflush(stdout))

#define PR(...)                   (fprintf(stdout, __VA_ARGS__))
#define COUNT_LIST_LENGTH(l)      ({ list_length_counter = 0; EACH((l), incr_list_length_counter); })
#define CORE_SETQ(env, sym, val)  ({                                                               \
      ae_obj_t * args = CONS(SYM(sym), CONS(val, NIL));                                            \
      (ae_core_setq(env, args, LENGTH(args)));                                                     \
    })

#define SETUP_TEST                                                                                 \
  ae_obj_t *    this    = NULL;                                                                    \
  ae_obj_t *    that    = NULL;                                                                    \
  symbols_list          = NIL;                                                                     \
  memset(mem, 0, free_list_size);                                                                  \
  free_list_reset();                                                                               \
  free_list_add_block(&mem[0], free_list_size);                                                    \
  pool_clear();                                                                                    \
  if (tmp_str) {                                                                                   \
    free(tmp_str);                                                                                 \
    tmp_str = NULL;                                                                                \
  }                                                                                                \
  (void)this;                                                                                      \
  (void)that;

#define DESCR(fun)                                                                                 \
  PR("\n\n[describe %s " #fun  "] ", GET_TYPE_STR(fun));                                           \
  LOG(FUN_PARAMS(fun), "params");                                                                  \
  LOG(FUN_ENV(fun), "env");                                                                        \
  LOG(FUN_BODY(fun), "body")

////////////////////////////////////////////////////////////////////////////////////////////////////
// Data
////////////////////////////////////////////////////////////////////////////////////////////////////

static int list_length_counter = 0;

static char * tmp_str = NULL;

////////////////////////////////////////////////////////////////////////////////////////////////////
// Helper functions
////////////////////////////////////////////////////////////////////////////////////////////////////

void before_acutest() {
  printf("\nobj size:     %d.\n",  sizeof(ae_obj_t));
  printf("int size:     %d.\n",    sizeof(int));
  printf("nil is at:    %016p.\n", NIL);
  printf("t is at:      %016p.\n", TRUE);
  printf("Pool first:   %016p.\n", pool_first);
  printf("Pool last:    %016p.\n", pool_last);
  printf("Pool size:    %016p (%zu bytes).\n",
         sizeof(ae_obj_t) * AE_OBJ_POOL_SIZE,
         sizeof(ae_obj_t) * AE_OBJ_POOL_SIZE);
  printf("Strings pool size: %016p (%zu bytes).", free_list_size, free_list_size);
  NL;
  NL;

  // This would be nice:
  /* if (! setopts(argc, argv)) { */
  /*   FPR(stderr, "ERROR: Bad opts!\n"); */
  /*   exit(1); */
  /* } */
}

char * princ_to_new_string(const ae_obj_t * const this) {
  char * buff;
  size_t size;
  FILE * stream = open_memstream(&buff, &size);

  FPRINC(this, stream);
  fclose(stream);

  return buff;
}

bool shitty_princ_based_equality_predicate(
  const ae_obj_t * const this,
  const char * const strcmp_str) {
  return ! strcmp(strcmp_str, SPRINC(this));
}

ae_obj_t * push_together_a_list_of_ints(void) {
  ae_obj_t * num     = NEW_INT(1);
  ae_obj_t * list    = NEW_CONS(num, NIL);
  ae_obj_t * tailtip = list;

  T(LENGTH(tailtip) == 1);

  for (int ix = 2; ix < 5; ix++) {
    int        int_val = ix;
    ae_obj_t * new_int = NEW_INT(int_val);

    tailtip            = PUSH(tailtip, new_int);

    T(CONSP(tailtip));
    T(INTEGERP(CAR(tailtip)));
    T(INT_VAL(CAR(tailtip)) == int_val);
    T(LENGTH(list) == ix);
    TM("Length is %zu.", LENGTH(tailtip));
  }

  return list;
}

ae_obj_t * cons_together_a_list_of_ints(void) {
  ae_obj_t * head = NEW_INT(4);
  ae_obj_t * list = NEW_CONS(head, NIL);

  T(LENGTH(list) == 1);

  for (unsigned int ix  = 0; ix < 3; ix++) {
    ae_obj_t * new_head = NEW_INT(3 - ix);
    ae_obj_t * tail     = list;

    list = CONS(new_head, tail);

    const int expected_length = 2 + ix;

    T(list != head);
    T(list != new_head);
    T(CAR(list) == new_head);
    T(CDR(list) == tail);
    T(LENGTH(list) == expected_length);
    TEST_MSG(
      "Incorrect length %d, expected %d.",
      LENGTH(list),
      expected_length);
  }

  return list;
}

void incr_list_length_counter(ae_obj_t * const this) {
  (void)this;
  list_length_counter++;
}

ae_obj_t * ae_obj_double(ae_obj_t * const this) {
  assert(INTEGERP(this));

  return NEW_INT(this->int_val * 2);
}

ae_obj_t * ae_obj_to_pairs(ae_obj_t * const this) {
  return CONS(this, NEW_CONS(this, NIL));
}

void basic_list_checks(ae_obj_t * this) {
  T(LENGTH(this)        == 4);

  COUNT_LIST_LENGTH(this);
  T(list_length_counter == 4);
  T(list_length_counter == LENGTH(this));

  T(shitty_princ_based_equality_predicate(this, "(1 2 3 4)"));

  ae_obj_t * mapped = NULL;

  mapped = MAP(this, ae_obj_double);
  T(shitty_princ_based_equality_predicate(mapped, "(2 4 6 8)"));
  tmp_str = SPRINC(this); TM("Got \"%s\".", tmp_str);

  mapped = CLONE(mapped);
  T(shitty_princ_based_equality_predicate(mapped, "(2 4 6 8)"));
  tmp_str = SPRINC(this); TM("Got \"%s\".", tmp_str);

  mapped = MAP(mapped, ae_obj_to_pairs);
  T(shitty_princ_based_equality_predicate(mapped, "((2 2) (4 4) (6 6) (8 8))"));
  tmp_str = SPRINC(this); TM("Got \"%s\".", tmp_str);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Tests
////////////////////////////////////////////////////////////////////////////////////////////////////

void remove_interned_symbol_from_list(void) {
  SETUP_TEST;

#define TEST_SYM(str)                                                                              \
  {                                                                                                \
    int len = LENGTH(symbols_list);                                                                \
    T(SYM(str) == SYM(str)));                                                                      \
  T(LENGTH(symbols_list) == (len + 1)));                                                           \
}

  // using 'symbols_list' as the symbol list here:
  T(SYM("a") == SYM("a"));
  T(LENGTH(symbols_list) == 1);
  T(SYM("b") == SYM("b"));
  T(LENGTH(symbols_list) == 2);
  T(SYM("c") == SYM("c"));
  T(LENGTH(symbols_list) == 3);
  T(SYM("d") == SYM("d"));
  T(LENGTH(symbols_list) == 4);

  that = SYM("b");

  // Add a duplicate element so that we can see that both instances are removed:
  symbols_list = CONS(that, symbols_list);

  T(LENGTH(symbols_list) == 5);
  T(MEMBERP(symbols_list, that));

  symbols_list = REMOVE(symbols_list, that);

  T(LENGTH(symbols_list) == 3);
  T(! MEMBERP(symbols_list, that));
  T(shitty_princ_based_equality_predicate(symbols_list, "(d c a)"));
}

void test_setup_is_okay(void)
{
  SETUP_TEST;
}

void newly_allocated_ae_obj_is_inside_pool(void)
{
  SETUP_TEST;
  this = ALLOC();
  T(this >= pool_first && this <= pool_last);
  TEST_MSG("obj @ %p is outside of pool (pool begins at %p, ends at %p).", this, pool_first, pool_last);
}

void newly_allocated_ae_obj_type_is_AE_INVALID(void)
{
  SETUP_TEST;
  T(INVALIDP(ALLOC()));
}

void newly_initialized_ae_obj_has_correct_type_field(void) {
#define test(_type)                                                                                \
  {                                                                                                \
    SETUP_TEST;                                                                                    \
    this = NEW(_type);                                                                             \
    T(GET_TYPE(this) == _type);                                                                    \
  }
  FOR_EACH_LEXED_TYPE(test);
}

void newly_initialized_ae_obj_has_zeroed_data_fields(void) {
  SETUP_TEST;
  this = NEW(AE_ENV);
  T(! ENV_PARENT(this));
  T(! ENV_SYMS  (this));
  T(! ENV_VALS  (this));
}

void consed_list_tests(void) {
  SETUP_TEST;
  basic_list_checks(cons_together_a_list_of_ints());
}

void pushed_list_tests(void) {
  SETUP_TEST;
  basic_list_checks(push_together_a_list_of_ints());
}

void unsafe_move_an_ae_obj(void) {
  SETUP_TEST;

  that = NEW(AE_RATIONAL);
  that->numerator_val   = 123;
  that->denominator_val = 321;

  this = MOVE_NEW(that);

  T(RATIONALP(this));
  T(this->numerator_val   == 123);
  T(this->denominator_val == 321);

  T(FREEP(that));
  T(that->numerator_val   == 0);
  T(that->denominator_val == 0);
}

void clone_a_simple_ae_obj(void) {
  SETUP_TEST;

  this = NEW(AE_RATIONAL);
  this->numerator_val   = 123;
  this->denominator_val = 321;

  that = CLONE(this);

  T(this != that);
  T(RATIONALP(that));
  T(that->numerator_val   == 123);
  T(that->denominator_val == 321);
}

void pushed_and_consed_lists_princ_identically(void) {
  SETUP_TEST;
  this    = push_together_a_list_of_ints();
  tmp_str = SPRINC(this);
  T(shitty_princ_based_equality_predicate(cons_together_a_list_of_ints(), tmp_str));
}

void intern_symbols(void) {
  SETUP_TEST;

  // deliberately avoid initializing symbols_list because SYM2 should do so itself automatically.

  T(! strcmp(SYM_VAL(SYM("one")), "one"));
  T(SYM("one") == SYM("one"));
  T(LENGTH(symbols_list) == 1);

  T(! strcmp(SYM_VAL(SYM("two")), "two"));
  T(! (SYM("one") == SYM("two")));
  T(LENGTH(symbols_list) == 2);
}

#define LENGTH_TEST(fun, expect, type, field, val, ...)                                            \
  {                                                                                                \
    this        = NEW(type);                                                                       \
    this->field = val;                                                                             \
                                                                                                   \
    __VA_ARGS__;                                                                                   \
                                                                                                   \
    char * buff;                                                                                   \
    size_t size;                                                                                   \
    FILE * stream   = open_memstream(&buff, &size);                                                \
    int    reported = ae_##fun(this, stream);                                                      \
                                                                                                   \
    fclose(stream);                                                                                \
    free(buff);                                                                                    \
                                                                                                   \
    if (strlen(buff) != expect)                                                                    \
    {                                                                                              \
      NL;                                                                                          \
      PR("With " #fun ",  <");                                                                     \
      PR(buff);                                                                                    \
      PR("> wrote %d characters, expected %d.", strlen(buff), expect);                             \
    }                                                                                              \
                                                                                                   \
    T(strlen(buff) == expect);                                                                     \
    T((int)strlen(buff) == (int)size);                                                             \
    TM("strlen was %d but size was %d:\n\"%s\".\n",                                                \
       (int)strlen(buff), (int)size, buff);                                                        \
    T((int)strlen(buff) == (int)reported);                                                         \
    TM("strlen was %d but reported was %d:\n\"%s\".\n",                                            \
       (int)strlen(buff), (int)reported, buff);                                                    \
  }

void fprinc_fwrite_lengths(void) {
  SETUP_TEST;

  LENGTH_TEST(fprinc, 1, AE_CHAR,     char_val,      '1'                                 );
  LENGTH_TEST(fwrite, 3, AE_CHAR,     char_val,      '1'                                 );
  LENGTH_TEST(fprinc, 2, AE_CHAR,     char_val,      '\n'                                );
  LENGTH_TEST(fwrite, 4, AE_CHAR,     char_val,      '\n'                                );
  LENGTH_TEST(fprinc, 3, AE_INTEGER,  int_val,       123                                 );
  LENGTH_TEST(fwrite, 3, AE_INTEGER,  int_val,       123                                 );
  LENGTH_TEST(fprinc, 4, AE_FLOAT,    float_val,     1.23                                );
  LENGTH_TEST(fwrite, 4, AE_FLOAT,    float_val,     1.23                                );
  LENGTH_TEST(fprinc, 7, AE_RATIONAL, numerator_val, 123,   this->denominator_val = 456; );
  LENGTH_TEST(fwrite, 7, AE_RATIONAL, numerator_val, 123,   this->denominator_val = 456; );
  LENGTH_TEST(fprinc, 4, AE_STRING,   str_val,       "asdf"                              );
  LENGTH_TEST(fwrite, 6, AE_STRING,   str_val,       "asdf"                              );
  LENGTH_TEST(fprinc, 4, AE_SYMBOL,   sym_val,       "ghij"                              );
  LENGTH_TEST(fwrite, 4, AE_SYMBOL,   sym_val,       "ghij"                              );
}

void eql(void) {
  SETUP_TEST;

  ae_obj_t * obj_int_2a      = NEW_INT(2);
  ae_obj_t * obj_int_2b      = NEW_INT(2);
  ae_obj_t * obj_float_2a    = NEW_FLOAT(2.0);
  ae_obj_t * obj_float_2b    = NEW_FLOAT(2.0);
  ae_obj_t * obj_int_3a      = NEW_INT(3);
  ae_obj_t * obj_int_3b      = NEW_INT(3);
  ae_obj_t * obj_float_3a    = NEW_FLOAT(3.0);
  ae_obj_t * obj_float_3b    = NEW_FLOAT(3.0);
  ae_obj_t * obj_bool_false  = TRUTH(false);
  ae_obj_t * obj_bool_true   = TRUTH(obj_float_2a);
  ae_obj_t * obj_list_consed = cons_together_a_list_of_ints();
  ae_obj_t * obj_list_pushed = push_together_a_list_of_ints();
  ae_obj_t * obj_rat_2a      = NEW_RATIONAL(2, 1);
  ae_obj_t * obj_rat_2b      = NEW_RATIONAL(4, 2);
  ae_obj_t * obj_rat_3a      = NEW_RATIONAL(3, 1);
  ae_obj_t * obj_rat_3b      = NEW_RATIONAL(9, 3);
  ae_obj_t * obj_char_a_a    = NEW_CHAR('a');
  ae_obj_t * obj_char_a_b    = NEW_CHAR('a');
  ae_obj_t * obj_char_b_a    = NEW_CHAR('b');
  ae_obj_t * obj_char_b_b    = NEW_CHAR('b');
  char     * pchar_a         = "a";
  ae_obj_t * obj_string_a_a  = NEW_STRING(pchar_a);
  ae_obj_t * obj_string_a_b  = NEW_STRING(pchar_a);
  ae_obj_t * obj_string_a_c  = NEW_STRING("a");
  ae_obj_t * obj_string_b_a  = NEW_STRING("b");

#define FOR_EVERY_OBJ_DO(X)                                                                        \
  X(  obj_int_2a)                                                                                  \
    X(obj_int_2b)                                                                                  \
    X(obj_float_2a)                                                                                \
    X(obj_float_2b)                                                                                \
    X(obj_int_3a)                                                                                  \
    X(obj_int_3b)                                                                                  \
    X(obj_float_3a)                                                                                \
    X(obj_float_3b)                                                                                \
    X(obj_bool_false)                                                                              \
    X(obj_bool_true)                                                                               \
    X(obj_list_consed)                                                                             \
    X(obj_list_pushed)                                                                             \
    X(obj_rat_2a)                                                                                  \
    X(obj_rat_2b)                                                                                  \
    X(obj_rat_3a)                                                                                  \
    X(obj_rat_3b)                                                                                  \
    X(obj_char_a_a)                                                                                \
    X(obj_char_a_b)                                                                                \
    X(obj_char_b_a)                                                                                \
    X(obj_char_b_b)                                                                                \
    X(obj_string_a_a)                                                                              \
    X(obj_string_a_b)                                                                              \
    X(obj_string_a_c)                                                                              \
    X(obj_string_b_a)

#define NETP(first, second)                                                                        \
  if (first != second) {                                                                           \
    T( ! EQL( (first)  , (second) ));                                                              \
    T( ! EQL( (second) , (first)  ));                                                              \
  }

#define ETP(first, second)                                                                         \
  T( EQL( (first)  , (second) ));                                                                  \
  T( EQL( (second) , (first)  ));

#define SELF_EQL(o)                                                                                \
  ETP( obj_bool_false , obj_bool_false );

  //  Everything is equal to itself.
  FOR_EVERY_OBJ_DO(SELF_EQL);

  // strings:
  ETP( obj_string_a_a, obj_string_a_b);
  ETP( obj_string_a_a, obj_string_a_c);
  NETP(obj_string_a_a, obj_string_b_a);

  // characters:
  ETP( obj_char_a_a  , obj_char_a_b);
  NETP(obj_char_a_a  , obj_char_b_a);

  //  Some numbers are equal to each other:
  ETP( obj_int_2a    , obj_int_2b   );
  ETP( obj_float_2a  , obj_float_2b );
  ETP( obj_rat_2a    , obj_rat_2b   );
  ETP( obj_int_3a    , obj_int_3b   );
  ETP( obj_float_3a  , obj_float_3b );
  ETP( obj_rat_3a    , obj_rat_3b   );
  // ... even if the y have different types:
  ETP( obj_int_2a    , obj_float_2a );
  ETP( obj_int_2a    , obj_rat_2b   );
  ETP( obj_rat_2a    , obj_float_2a );
  ETP( obj_int_3a    , obj_float_3a );
  ETP( obj_int_3a    , obj_rat_3b   );
  ETP( obj_rat_3a    , obj_float_3a );

  //  Some numbers are not equal to each other.
  NETP( obj_int_2a   , obj_int_3a   );
  NETP( obj_int_2a   , obj_float_3a );
  NETP( obj_int_2a   , obj_int_3a   );
  NETP( obj_int_2a   , obj_rat_3a   );

  NETP( obj_float_2a , obj_int_3a   );
  NETP( obj_float_2a , obj_float_3a );
  NETP( obj_float_2a , obj_int_3a   );
  NETP( obj_float_2a , obj_rat_3a   );

  NETP( obj_rat_2a   , obj_int_3a   );
  NETP( obj_rat_2a   , obj_float_3a );
  NETP( obj_rat_2a   , obj_int_3a   );
  NETP( obj_rat_2a   , obj_rat_3a   );

  // These aren't equal to anything other than themselves:
#define XX(other) NETP(obj_bool_false, other);
  FOR_EVERY_OBJ_DO(XX);
#undef XX

#define XX(other) NETP(obj_bool_true, other);
  FOR_EVERY_OBJ_DO(XX);
#undef XX

#define XX(other) NETP(obj_list_consed, other);
  FOR_EVERY_OBJ_DO(XX);
#undef XX

#define XX(other) NETP(obj_list_pushed, other);
  FOR_EVERY_OBJ_DO(XX);
#undef XX
}

void truth(void) {
  SETUP_TEST;

  this = ae_obj_truth(true);

  T(SYMBOLP(this) && ! strcmp(SYM_VAL(this), "t"));

  that = ae_obj_truth(false);

  T(NILP(that));
}

#define ENV_TRIO                                                                                   \
  ae_obj_t * root   = ENV_NEW_ROOT();                                                              \
  ae_obj_t * parent = NEW_ENV(root,   NIL, NIL);                                                   \
  ae_obj_t * child  = NEW_ENV(parent, NIL, NIL);
  

void env_scoping(void) {
  SETUP_TEST;
  {
    ENV_TRIO;

    /**/ENV_SET(     parent, SYM("foo"), SYM("bar"));
    T(  ENV_BOUNDP(  root,   SYM("foo")));
    T(  ENV_BOUNDP(  parent, SYM("foo")));
    T(  ENV_BOUNDP(  child,  SYM("foo")));
    T(  ENV_BOUNDP_L(root,   SYM("foo")));
    T(! ENV_BOUNDP_L(parent, SYM("foo")));
    T(! ENV_BOUNDP_L(child,  SYM("foo")));

    /**/ENV_SET_L(   child, SYM("foo"), SYM("baz"));
    T(  ENV_BOUNDP(  root,   SYM("foo")));
    T(  ENV_BOUNDP(  parent, SYM("foo")));
    T(  ENV_BOUNDP(  child,  SYM("foo")));
    T(  ENV_BOUNDP_L(root,   SYM("foo")));
    T(! ENV_BOUNDP_L(parent, SYM("foo")));
    T(  ENV_BOUNDP_L(child,  SYM("foo")));
  }
  {
    ENV_TRIO;

    /**/ENV_SET_L(   parent, SYM("foo"), SYM("bar"));
    T(! ENV_BOUNDP(  root,   SYM("foo")));
    T(  ENV_BOUNDP(  parent, SYM("foo")));
    T(  ENV_BOUNDP(  child,  SYM("foo")));
    T(! ENV_BOUNDP_L(root,   SYM("foo")));
    T(  ENV_BOUNDP_L(parent, SYM("foo")));
    T(! ENV_BOUNDP_L(child,  SYM("foo")));
  }
  {
    ENV_TRIO;

    /**/ENV_SET_G(   parent, SYM("foo"), SYM("bar"));
    T(  ENV_BOUNDP(  root,   SYM("foo")));
    T(  ENV_BOUNDP(  parent, SYM("foo")));
    T(  ENV_BOUNDP(  child,  SYM("foo")));
    T(  ENV_BOUNDP_L(root,   SYM("foo")));
    T(! ENV_BOUNDP_L(parent, SYM("foo")));
    T(! ENV_BOUNDP_L(child,  SYM("foo")));
    
    /**/ENV_SET(     child,  SYM("foo"), SYM("baz"));
    T(  ENV_BOUNDP(  root,   SYM("foo")));
    T(  ENV_BOUNDP(  parent, SYM("foo")));
    T(  ENV_BOUNDP(  child,  SYM("foo")));
    T(  ENV_BOUNDP_L(root,   SYM("foo")));
    T(! ENV_BOUNDP_L(parent, SYM("foo")));
    T(! ENV_BOUNDP_L(child,  SYM("foo")));

    /**/ENV_SET_L(   child,  SYM("foo"), SYM("quux"));
    T(  ENV_BOUNDP(  root,   SYM("foo")));
    T(  ENV_BOUNDP(  parent, SYM("foo")));
    T(  ENV_BOUNDP(  child,  SYM("foo")));
    T(  ENV_BOUNDP_L(root,   SYM("foo")));
    T(! ENV_BOUNDP_L(parent, SYM("foo")));
    T(  ENV_BOUNDP_L(child,  SYM("foo")));
  }
}

void env_basics(void) {
  SETUP_TEST;

  this = NEW_ENV(NIL, NIL, NIL);

  T(LENGTH(ENV_SYMS(this)) == 0);
  T(LENGTH(ENV_VALS(this)) == 0);
  T(NILP(ENV_SYMS(this)));
  T(NILP(ENV_VALS(this)));

  T(! MEMBERP(ENV_SYMS(this), SYM("foo")));

  ENV_ADD(this, SYM("foo"), NEW_INT(12));

  T(LENGTH(ENV_SYMS(this)) == 1);
  T(LENGTH(ENV_VALS(this)) == 1);
  T(MEMBERP(ENV_SYMS(this), SYM("foo")));
  T(! NILP(ENV_SYMS(this)));
  T(! NILP(ENV_VALS(this)));

  T(! MEMBERP(ENV_SYMS(this), SYM("bar")));

  ENV_ADD(this, SYM("bar"), NEW_INT(24));

  T(LENGTH(ENV_SYMS(this)) == 2);
  T(LENGTH(ENV_VALS(this)) == 2);
  T(MEMBERP(ENV_SYMS(this), SYM("bar")));

  T(! MEMBERP(ENV_SYMS(this), SYM("baz")));

  ENV_ADD(this, SYM("baz"), NEW_INT(36));

  T(LENGTH(ENV_SYMS(this)) == 3);
  T(LENGTH(ENV_VALS(this)) == 3);
  T(MEMBERP(ENV_SYMS(this), SYM("baz")));
  T(INT_VAL(ENV_FIND(this, SYM("foo"))) == 12);
  T(INT_VAL(ENV_FIND(this, SYM("bar"))) == 24);
  T(INT_VAL(ENV_FIND(this, SYM("baz"))) == 36);

  that = NEW_ENV(NIL, NIL, NIL); // not yet linked to.

  ENV_ADD(that, SYM("quux"), NEW_INT(48));

  T(NILP(ENV_FIND(this, SYM("quux"))));

  ENV_PARENT(this) = that; // link this to that.

  T(INT_VAL(ENV_FIND(this, SYM("quux"))) == 48);
  T(ENV_FIND(this, SYM("quux")) == ENV_FIND(that, SYM("quux")));
  T(NILP(ENV_FIND(this, SYM("zot"))));
  T(NILP(ENV_FIND(that, SYM("foo"))));

  ENV_SET(this, SYM("bar"), NEW_INT(99));

  T(INT_VAL(ENV_FIND(this, SYM("bar"))) == 99);

  ENV_SET(this, SYM("zot"), NEW_INT(66));

  T(INT_VAL(ENV_FIND(this, SYM("zot"))) == 66);

#ifdef AE_LOG_ENV_TEST
  pool_print();
  PR("Upper:\n");
  PR("  syms ");
  PRINC(ENV_SYMS(that));
  NL;
  PR("  vals ");
  PRINC(ENV_VALS(that));
  NL;
  PR("Lower:\n");
  PR("  syms ");
  PRINC(ENV_SYMS(this));
  NL;
  PR("  vals ");
  PRINC(ENV_VALS(this));
  NL;
#endif
}

void improper_list(void) {
  SETUP_TEST;

  this = CONS(NEW_INT(1), CONS(NEW_INT(2), NEW_CONS(NEW_INT(3), NEW_INT(4))));
  // OLOG(this); NL;
  T(LENGTH(this)        == 3);
  TM("Expected length 3, got %d.", LENGTH(this));

  COUNT_LIST_LENGTH(this);
  T(list_length_counter == 3);

  T(shitty_princ_based_equality_predicate(this, "(1 2 3 . 4)"));

  ae_obj_t * mapped = MAP(this, ae_obj_double);
  T(shitty_princ_based_equality_predicate(mapped, "nil"));
  T(NILP(mapped));

  // PUT(NEW_CONS(NEW_INT(1), NEW_INT(2)));
}

ae_obj_t * make_args_containing_one_list(void) {
  return NEW_CONS(CONS(SYM("a"), CONS(SYM("b"), NEW_CONS(SYM("c"), NIL))), NIL);
}

ae_obj_t * make_args_for_cons(void) {
  return CONS(NIL, NEW_CONS(CONS(SYM("a"), CONS(SYM("b"), NEW_CONS(SYM("c"), NIL))), NIL));
}

void fun_specialness(void) {
  SETUP_TEST;
  ae_obj_t * env   = ENV_NEW_ROOT();

  T(COREP(ENV_FIND(env, SYM("progn"))));
  T(SPECIALP(ENV_FIND(env, SYM("progn"))));

  T(COREP(ENV_FIND(env, SYM("if"))));
  T(SPECIALP(ENV_FIND(env, SYM("if"))));

  T(COREP(ENV_FIND(env, SYM("cond"))));
  T(SPECIALP(ENV_FIND(env, SYM("cond"))));

  T(COREP(ENV_FIND(env, SYM("lambda"))));
  T(SPECIALP(ENV_FIND(env, SYM("lambda"))));

  T(COREP(ENV_FIND(env, SYM("let"))));
  T(SPECIALP(ENV_FIND(env, SYM("let"))));

  T(COREP(ENV_FIND(env, SYM("print"))));
  T(! SPECIALP(ENV_FIND(env, SYM("print"))));

  T(COREP(ENV_FIND(env, SYM("cons"))));
  T(! SPECIALP(ENV_FIND(env, SYM("cons"))));

  T(COREP(ENV_FIND(env, SYM("car"))));
  T(! SPECIALP(ENV_FIND(env, SYM("car"))));

  T(COREP(ENV_FIND(env, SYM("cdr"))));
  T(! SPECIALP(ENV_FIND(env, SYM("cdr"))));

  T(COREP(ENV_FIND(env, SYM("+"))));
  T(! SPECIALP(ENV_FIND(env, SYM("+"))));

  T(COREP(ENV_FIND(env, SYM("=="))));
  T(! SPECIALP(ENV_FIND(env, SYM("=="))));
}

void core_cons_car_cdr(void) {
  SETUP_TEST;
  ae_obj_t * env   = ENV_NEW_ROOT();

  ae_obj_t * args = make_args_containing_one_list();
  T(ae_core_car(env, args, LENGTH(args)) == SYM("a"));

  T(shitty_princ_based_equality_predicate(ae_core_cdr (env, args, LENGTH(args)), "(b c)"));

  args = NEW_CONS(ae_core_cdr(env, args, LENGTH(args)), NIL);
  T(ae_core_car(env, args, LENGTH(args)) == SYM("b"));

  args = CONS(SYM("a"), NEW_CONS(NIL, NIL));
  T(shitty_princ_based_equality_predicate(ae_core_cons(env, args, LENGTH(args)), "(a)")); // cons 'a onto nil and get (a).

  args = make_args_for_cons();
  T(shitty_princ_based_equality_predicate(ae_core_cons(env, args, LENGTH(args)), "(nil a b c)" ));

  args = NEW_CONS(NIL, NIL);
  T(NILP(ae_core_car(env, args, LENGTH(args))));
  T(NILP(ae_core_cdr(env, args, LENGTH(args))));

  args = NEW_CONS(NIL, NIL);
  args = NEW_CONS(ae_core_car(env, args, LENGTH(args)), NIL);
  T(NILP(ae_core_car(env, args, LENGTH(args))));
  T(NILP(ae_core_cdr(env, args, LENGTH(args))));
  T(NILP(ae_core_car(env, args, LENGTH(args))));
  T(NILP(ae_core_cdr(env, args, LENGTH(args))));
}

void core_eq_eql_not(void) {
  SETUP_TEST;
  ae_obj_t * env   = ENV_NEW_ROOT();

  this = CONS(NEW_INT(1), NEW_CONS(NEW_INT(2), NIL));
  that = CONS(NEW_INT(1), NEW_CONS(NEW_INT(2), NIL));

  ae_obj_t * args = NIL;
  
  args = CONS(NEW_INT(5), NEW_CONS(NEW_INT  (5  ), NIL));
  T(TRUEP(ae_core_eql  (env, args, LENGTH(args)))); // 5 and 5 are equal numbers...
  args = CONS(NEW_INT(5), NEW_CONS(NEW_INT  (5  ), NIL));
  T(NILP (ae_core_eq   (env, args, LENGTH(args)))); // ... but they are not the same object.
  args = CONS(NEW_INT(5), NEW_CONS(NEW_FLOAT(5.0), NIL));
  T(TRUEP(ae_core_eql  (env, args, LENGTH(args)))); // 5 and 5.0 are equal-enough numbers...
  args = CONS(NEW_INT(5), NEW_CONS(NEW_INT  (6  ), NIL));
  T(NILP (ae_core_eql  (env, args, LENGTH(args)))); // ... but 5 and 6 are not.

  args = CONS(this      , NEW_CONS(this, NIL));
  T(TRUEP(ae_core_eq   (env, args, LENGTH(args)))); // These are the same object and so are eq
  args = CONS(this      , NEW_CONS(this, NIL));
  T(TRUEP(ae_core_eql  (env, args, LENGTH(args)))); // and also eql.
  args = CONS(this      , NEW_CONS(that, NIL));
  T(NILP (ae_core_eq   (env, args, LENGTH(args)))); // These are the NOT the same object and so
  args = CONS(this      , NEW_CONS(that, NIL));
  T(NILP (ae_core_eql  (env, args, LENGTH(args)))); // neither eq or eql.
  args = CONS(that      , NEW_CONS(this, NIL));
  T(NILP (ae_core_eq   (env, args, LENGTH(args)))); // eq is commutative.
  args = CONS(that      , NEW_CONS(this, NIL));
  T(NILP (ae_core_eql  (env, args, LENGTH(args)))); // eql too.

  args = CONS(this      , CONS(this         , NEW_CONS(this      , NIL)));
  T(TRUEP(ae_core_eq   (env, args, LENGTH(args)))); // eq can take 3+ arguments...
  args = CONS(this      , CONS(this         , NEW_CONS(that      , NIL)));
  T(NILP (ae_core_eq   (env, args, LENGTH(args))));
  args = CONS(NIL       , CONS(NIL          , NEW_CONS(NIL       , NIL)));
  T(TRUEP(ae_core_eq   (env, args, LENGTH(args))));
  args = CONS(NIL       , CONS(NIL          , NEW_CONS(TRUE      , NIL)));
  T(NILP (ae_core_eq   (env, args, LENGTH(args))));

  args = CONS(NEW_INT(5), CONS(NEW_INT(5)   , NEW_CONS(NEW_INT(5), NIL)));
  T(TRUEP(ae_core_eql  (env, args, LENGTH(args)))); // ...so can eql.
  args = CONS(NEW_INT(5), CONS(NEW_INT(5)   , NEW_CONS(NEW_INT(6), NIL)));
  T(NILP (ae_core_eql  (env, args, LENGTH(args))));

  args = CONS(NIL       , CONS(NIL          , NEW_CONS(NIL       , NIL)));
  T(TRUEP(ae_core_not  (env, args, LENGTH(args))));
  args = CONS(NIL       , CONS(NIL          , NEW_CONS(TRUE      , NIL)));
  T(NILP (ae_core_not  (env, args, LENGTH(args))));
}

void core_print_princ_write(void) {
  SETUP_TEST;
  ae_obj_t * env   = ENV_NEW_ROOT();

  NL;
  {
    PR("write-ing '\"hello\" 5 a abc' on the next line, with quoting: ");
    NL;

    ae_obj_t * args    = CONS(NEW_STRING("hello"),
                              CONS(NEW_INT(5),
                                   CONS(NEW_CHAR('a'),
                                        CONS(SYM("abc"),
                                             NIL))));
    ae_obj_t * written = ae_core_write(env, args, LENGTH(args));
    NL;
    T(INT_VAL(written) == 17);
    TM("Expected %d, wrote %d.", 17, INT_VAL(written));
  }
  {
    PR("\nprint-ing \"hello\",  5 a, abc on the next 3 lines, with quoting: ");
    ae_obj_t * args    = CONS(NEW_STRING("hello"),
                              CONS(NEW_INT(5),
                                   CONS(NEW_CHAR('a'),
                                        CONS(SYM("abc"),
                                             NIL))));
    ae_obj_t * written = ae_core_print(env, args, LENGTH(args));
    NL;
    T(INT_VAL(written) == 21);
    TM("Expected %d, wrote %d.", 14, INT_VAL(written));
  }
  {
    PR("\nprinc-ing 'hello5aabc' on the next line, without quoting: ");
    NL;
    ae_obj_t * args    = CONS(NEW_STRING("hello"),
                              CONS(NEW_INT(5),
                                   CONS(NEW_CHAR('a'),
                                        CONS(SYM("abc"),
                                             NIL))));
    ae_obj_t * written = ae_core_princ(env, args, LENGTH(args));;
    NL;
    T(INT_VAL(written) == 10);
    TM("Expected %d, wrote %d.", 10, INT_VAL(written));
  }
  NL;
}

void core_math(void) {
  SETUP_TEST;
  ae_obj_t * env  = ENV_NEW_ROOT();
  ae_obj_t * args = NIL;
  
  args = CONS(NEW_INT(24), CONS(NEW_INT(4), NEW_CONS(NEW_INT(3) , NIL)));
  this = ae_core_add(env, args, LENGTH(args));
  T(EQL(this, NEW_INT(31)));

  args = CONS(NEW_INT(24), CONS(NEW_INT(4), NEW_CONS(NEW_INT(3) , NIL)));
  this = ae_core_sub(env, args, LENGTH(args));
  T(EQL(this, NEW_INT(17)));

  args = CONS(NEW_INT(3),  CONS(NEW_INT(4), NEW_CONS(NEW_INT(24), NIL)));
  this = ae_core_sub(env, args, LENGTH(args));
  T(EQL(this, NEW_INT(-25)));

  args = CONS(NEW_INT(24), CONS(NEW_INT(4), NEW_CONS(NEW_INT(3) , NIL)));
  this = ae_core_mul(env, args, LENGTH(args));
  T(EQL(this, NEW_INT(288)));

  args = CONS(NEW_INT(24), CONS(NEW_INT(4), NEW_CONS(NEW_INT(3) , NIL)));
  this = ae_core_div(env, args, LENGTH(args));
  T(EQL(this, NEW_INT(2)));
}

void core_cmp(void) {
  SETUP_TEST;
  ae_obj_t * env  = ENV_NEW_ROOT();
  ae_obj_t * args = NIL;
  
  args = CONS(NEW_INT(2), CONS(NEW_INT(2), NEW_CONS(NEW_INT(2), NIL)));
  T(TRUEP(ae_core_equal (env, args, LENGTH(args))));
  args = CONS(NEW_INT(2), CONS(NEW_INT(2), NEW_CONS(NEW_INT(3), NIL)));
  T(NILP (ae_core_equal (env, args, LENGTH(args))));

  args = CONS(NEW_INT(2), CONS(NEW_INT(2), NEW_CONS(NEW_INT(2), NIL)));
  T(NILP (ae_core_nequal(env, args, LENGTH(args))));
  args = CONS(NEW_INT(2), CONS(NEW_INT(2), NEW_CONS(NEW_INT(3), NIL)));
  T(TRUEP(ae_core_nequal(env, args, LENGTH(args))));

  args = CONS(NEW_INT(2), CONS(NEW_INT(4), NEW_CONS(NEW_INT(6), NIL)));
  T(TRUEP(ae_core_lt (env, args, LENGTH(args))));
  args = CONS(NEW_INT(2), CONS(NEW_INT(4), NEW_CONS(NEW_INT(6), NIL)));
  T(NILP (ae_core_gt (env, args, LENGTH(args))));

  args = CONS(NEW_INT(6), CONS(NEW_INT(4), NEW_CONS(NEW_INT(2), NIL)));
  T(TRUEP(ae_core_gt (env, args, LENGTH(args))));
  args = CONS(NEW_INT(6), CONS(NEW_INT(4), NEW_CONS(NEW_INT(2), NIL)));
  T(NILP (ae_core_lt (env, args, LENGTH(args))));

  args = CONS(NEW_INT(2), CONS(NEW_INT(4), CONS(NEW_INT(4), NEW_CONS(NEW_INT(6), NIL))));
  T(TRUEP(ae_core_lte(env, args, LENGTH(args))));
  args = CONS(NEW_INT(2), CONS(NEW_INT(4), CONS(NEW_INT(4), NEW_CONS(NEW_INT(6), NIL))));
  T(NILP (ae_core_gte(env, args, LENGTH(args))));

  args = CONS(NEW_INT(6), CONS(NEW_INT(4), CONS(NEW_INT(4), NEW_CONS(NEW_INT(2), NIL))));
  T(TRUEP(ae_core_gte(env, args, LENGTH(args))));
  args = CONS(NEW_INT(6), CONS(NEW_INT(4), CONS(NEW_INT(4), NEW_CONS(NEW_INT(2), NIL))));
  T(NILP (ae_core_lte(env, args, LENGTH(args))));
}

void core_sleep(void) {
  SETUP_TEST;
  ae_obj_t * env    = ENV_NEW_ROOT();

  ENV_SET(env, SYM("xx"), NEW_INT(0));
  
  ae_obj_t * expr   = NIL;
  ae_obj_t * add    = CONS(CONS(SYM("+"), CONS(SYM("xx"), CONS(NEW_INT(2), NIL))), NIL);
  ae_obj_t * incr2  = CONS(SYM("setq!"),   CONS(SYM("xx"), add));
  ae_obj_t * print  = CONS(SYM("print"),  CONS(SYM("xx"), NIL));
  ae_obj_t * sleep = CONS(SYM("sleep"),  CONS(NEW_INT(100), NIL));

  EVAL(env, CONS(SYM("setq!"), CONS(SYM("xx"), CONS(NEW_INT(10), NIL))));

  for (int ix = 0; ix < 6; ix++) {
    expr            = CONS(incr2, expr);
    expr            = CONS(sleep, expr);
    expr            = CONS(print, expr);
  }

  expr              = CONS(SYM("progn"), expr);

  NL; NL; PR("Counting from 10 to 20 (in steps of 2), 1/10 of a second apart.");
  EVAL(env, expr);
  NL;
  T(EQL(EVAL(env, SYM("xx")), NEW_INT(22)));
  NL;
}

void root_env_and_eval(void) {
  SETUP_TEST;

  NL;

  ae_obj_t * env    = ENV_NEW_ROOT();
  ae_obj_t * listf  = EVAL(env, SYM("list"));

  OLOG(listf);

  ae_obj_t * expr   = NIL;
  ae_obj_t * rtrn   = NIL;

  NL;
  PR("syms: "); WRITE(ENV_SYMS(env)); NL;
  PR("vals: "); WRITE(ENV_VALS(env)); NL;

  CORE_SETQ(env, "foo", NEW_INT(666));

  T(ERRORP(CORE_SETQ(env, "nil",      NEW_INT(111))));
  T(ERRORP(CORE_SETQ(env, "t",        NEW_INT(222))));
  T(ERRORP(CORE_SETQ(env, ":keyword", NEW_INT(333))));
  
  T(EQL(NEW_INT(25),  EVAL(env, CONS(SYM("+"), CONS(NEW_INT(16), NEW_CONS(NEW_INT(9), NIL))))));
  T(EQL(NEW_INT(672), EVAL(env, CONS(SYM("+"), CONS(NEW_INT(6), NEW_CONS(SYM("foo"), NIL))))));
  TM("Expected %d, got %d.", 672, INT_VAL(EVAL(env, CONS(SYM("+"), CONS(NEW_INT(6), NEW_CONS(SYM("foo"), NIL))))));

  T(EQL(NEW_INT(75),  EVAL(env, CONS(SYM("*"), CONS(NEW_INT(3),  NEW_CONS(CONS(SYM("+"), CONS(NEW_INT(16), NEW_CONS(NEW_INT(9), NIL))), NIL))))));

  EVAL(env, CONS(SYM("setq!"), CONS(SYM("bar"), NEW_CONS(NEW_INT(9), NIL))));
  EVAL(env, CONS(SYM("setq!"), CONS(SYM("baz"), NEW_CONS(CONS(SYM("+"), CONS(NEW_INT(16), NEW_CONS(NEW_INT(9), NIL))), NIL))));

  T(EQL(NEW_INT(9),   EVAL(env, SYM("bar"))));
  T(EQL(NEW_INT(25),  EVAL(env, SYM("baz"))));

  expr = CONS(SYM("progn"), CONS(CONS(SYM("princ"), NEW_CONS(NEW_STRING("Hello "), NIL)), CONS(CONS(SYM("princ"), NEW_CONS(NEW_STRING("from Ash"), NIL)), NEW_CONS(CONS(SYM("princ"), NEW_CONS(NEW_STRING("Lisp!"), NIL)), NIL))));

  NL;
  PR("Should Print \"Hello from Ash Lisp!\" on the next line:\n");
  this = EVAL(env, expr);
  T(EQL(NEW_INT(5), this));
  NL;

  expr = CONS(SYM("quote"), NEW_CONS(CONS(NEW_INT(5), CONS(NEW_INT(10), NEW_CONS(NEW_INT(15), NIL))), NIL));
  T(shitty_princ_based_equality_predicate(EVAL(env, expr), "(5 10 15)"));

  expr = CONS(SYM("quote"), NEW_CONS(SYM("a"), NIL));
  T(shitty_princ_based_equality_predicate(EVAL(env, expr), "a"));

  expr = CONS(SYM("if"), CONS(SYM("t"), CONS(NEW_INT(11), CONS(KW("ignored"), NEW_CONS(NEW_INT(22), NIL)))));
  T(EQL(NEW_INT(11), EVAL(env, expr)));

  expr = CONS(SYM("if"), CONS(SYM("t"), NEW_CONS(NEW_INT(11), NIL)));
  T(EQL(NEW_INT(11), EVAL(env, expr)));

  expr = CONS(SYM("if"), CONS(SYM("nil"), CONS(NEW_INT(11), CONS(KW("ignored"), NEW_CONS(NEW_INT(22), NIL)))));
  T(EQL(NEW_INT(22), EVAL(env, expr)));

  expr = CONS(SYM("if"), CONS(SYM("nil"), NEW_CONS(NEW_INT(11), NIL)));
  T(NILP(EVAL(env, expr)));
 
  expr = CONS(SYM("lambda"),
              CONS(NEW_CONS(SYM("x"), NIL),
                   CONS(
                     CONS(SYM("princ"), NEW_CONS(SYM("x"), NIL)), NEW_CONS(
                       CONS(SYM("+"),
                            CONS(SYM("x"), NEW_CONS(NEW_INT(2), NIL))), NIL))));

  rtrn = EVAL(env, expr);

  T(LAMBDAP(rtrn));

  ae_obj_t * subexpr = CONS(SYM("*"), CONS(NEW_INT(53), NEW_CONS(NEW_INT(54), NIL)));

  expr = CONS(CONS(SYM("lambda"),
                   CONS(NEW_CONS(SYM("x"), NIL),
                        CONS(CONS(SYM("princ"),
                                  NEW_CONS(SYM("x"), NIL)),
                             NEW_CONS(CONS(SYM("+"),
                                           CONS(SYM("x"),
                                                NEW_CONS(subexpr, NIL))), NIL)))),
              NEW_CONS(NEW_INT(31), NIL));


  PR("\nShould Print 31 on the next line:\n");
  rtrn = EVAL(env, expr);
  NL;

  // no princ:
  expr = CONS(CONS(SYM("lambda"),
                   CONS(NEW_CONS(SYM("x"), NIL),
                        NEW_CONS(CONS(SYM("+"),
                                      CONS(SYM("x"),
                                           NEW_CONS(NEW_INT(1), NIL))), NIL))),
              NEW_CONS(CONS(SYM("+"),
                            CONS(NEW_INT(2),
                                 NEW_CONS(NEW_INT(3), NIL))), NIL));

  PR("\nShould Print 6 on the next line:\n");
  rtrn = EVAL(env, expr);
  WRITE(rtrn);
  NL;

  T(EQL(NEW_INT(6), rtrn));

  ae_obj_t *    expr1 = CONS(CONS(SYM("=="), CONS(SYM("a"), NEW_CONS(NEW_INT(1), NIL))), NEW_CONS(NEW_INT(10), NIL));
  ae_obj_t *    expr2 = CONS(CONS(SYM("=="), CONS(SYM("a"), NEW_CONS(NEW_INT(2), NIL))), NEW_CONS(NEW_INT(20), NIL));
  ae_obj_t *    expr3 = CONS(TRUE, NEW_CONS(NEW_INT(30), NIL));
  /*   */ expr = CONS(SYM("cond"), CONS(expr1, CONS(expr2, NEW_CONS(expr3, NIL))));

  NL;
  PR("Evaluating this cond: ");
  WRITE(expr);
  PR(".");
  NL;

#define TEST_COND(input, expected)                                                                 \
  {                                                                                                \
    CORE_SETQ(env, "a", NEW_INT(input));                                                           \
    this = EVAL(env, expr);                                                                        \
    PR("Rtrn for " #input " is ");                                                                 \
    PRINC(this);                                                                                   \
    PR(".");                                                                                       \
    NL;                                                                                            \
    if (! EQL(this, NEW_INT(expected))) {                                                          \
      NL;                                                                                          \
      PR("this ");                                                                                 \
      WRITE(this);                                                                                 \
      PR(" == expected ");                                                                         \
      WRITE(NEW_INT(expected));                                                                    \
      NL;                                                                                          \
    }                                                                                              \
    T(EQL(this, NEW_INT(expected)));                                                               \
  }

  TEST_COND(1, 10);
  TEST_COND(2, 20);
  TEST_COND(3, 30);

  NL;
}

void list_fun(void) {
  SETUP_TEST;

  ae_obj_t * env           = ENV_NEW_ROOT();
  ae_obj_t * list_fun      = ENV_FIND(env, SYM("list"));
  ae_obj_t * list_fun_call = CONS(list_fun, CONS(NEW_INT(1), CONS(NEW_INT(2), NEW_CONS(NEW_INT(3), NIL))));
  ae_obj_t * ret           = EVAL(env, list_fun_call);

  T(shitty_princ_based_equality_predicate(ret, "(1 2 3)"));
  NL;
}

ae_obj_t * apply_user_fun(ae_obj_t * fun, ae_obj_t * env, ae_obj_t * args);

#define GENERATED_MACRO_TEST(name, expect_str)                                                     \
  {                                                                                                \
    tmp_str = SPRINC(ae_generate_macro_ ## name());                                                \
                                                                                                   \
    if (strcmp(tmp_str, expect_str)) {                                                             \
      NL;                                                                                          \
      PR("Got      %s\n", tmp_str);                                                                \
      PR("Wanted   %s\n", expect_str);                                                             \
    }                                                                                              \
                                                                                                   \
    T(! strcmp(tmp_str, expect_str));                                                              \
    free(tmp_str);                                                                                 \
    tmp_str = NULL;                                                                                \
  }

void macro_expand(void) {
  SETUP_TEST;

  PR("\n\nPopulating root env...");
  ae_obj_t * env = ENV_NEW_ROOT();
  PR("\nDone populating root env.\n");

  GENERATED_MACRO_TEST(defmacro, "(setq! defmacro (macro (name params . body) (list 'setq! name (list 'macro params . body))))");
  GENERATED_MACRO_TEST(defun,    "(defmacro defun (name params . body) (list 'setq! name (list 'lambda params . body)))");
  GENERATED_MACRO_TEST(and,      "(defmacro and args (cond ((null args) t) ((null (cdr args)) (car args)) (t (list 'if (car args) (cons 'and (cdr args))))))");
  GENERATED_MACRO_TEST(or,       "(defmacro or args (if (null args) nil (cons 'cond (mapcar list args))))");

  ae_obj_t * macro_def = NIL;
  macro_def = CONS(CONS(SYM("quote"), CONS(SYM("+"), NIL)), CONS(SYM("xxx"), CONS(SYM("yyy"), macro_def)));
  macro_def = CONS(CONS(SYM("list"), macro_def), NIL);
  macro_def = CONS(CONS(SYM("xxx"), CONS(SYM("yyy"), NIL)),  macro_def);
  macro_def = CONS(SYM("macro"), macro_def);
  PR("macro def  "); PRINC(macro_def); NL;
  PR("should be  (macro (xxx yyy) (list (quote +) xxx yyy))");
  T(shitty_princ_based_equality_predicate(macro_def, "(macro (xxx yyy) (list (quote +) xxx yyy))"));

  ae_obj_t * setq_for_macro_def   = CONS(SYM("setq!"), CONS(SYM("add2"), CONS(macro_def, NIL)));
  ae_obj_t * rtrn_for_macro_def   = EVAL(env, setq_for_macro_def);
  NL;
  OLOG(setq_for_macro_def);
  OLOG(rtrn_for_macro_def);
  NL;

  ae_obj_t * call_add2      = CONS(SYM("add2"), CONS(NEW_INT(5), CONS(NEW_INT(8), NIL)));
  OLOG(call_add2);
  NL;

  ae_obj_t * call_add2_rtrn = EVAL(env, call_add2);
  OLOG(call_add2_rtrn);
  NL;

  ae_obj_t * eval_call_add2_rtrn = EVAL(env, call_add2_rtrn);
  OLOG(eval_call_add2_rtrn);
  NL;

  /* { */
  /*   ae_obj_t * princ = CONS(SYM("princ"),  CONS(CONS(SYM("quote"), CONS(CONS(SYM("hello"), CONS(NEW_STRING("hello"), NIL)), NIL)), NIL)   ); */
  /*   NL; */
  /*   OLOG(princ); */
  /*   NL; */
  /*   ae_obj_t * princed_len = EVAL(env, princ); */
  /*   NL; */
  /*   OLOG(princed_len); */
  /*   NL; */
  /* } */

  /* { */
  /*   ae_obj_t * princ = CONS(SYM("print"),  CONS(CONS(SYM("quote"), CONS(CONS(SYM("hello"), CONS(NEW_STRING("hello"), NIL)), NIL)), NIL)   ); */
  /*   NL; */
  /*   OLOG(princ); */
  /*   NL; */
  /*   ae_obj_t * princed_len = EVAL(env, princ); */
  /*   NL; */
  /*   OLOG(princed_len); */
  /*   NL; */
  /* } */
}

void deloc(void) {
  SETUP_TEST;

  ae_obj_t * an_int = NEW_INT(14);
  
  T(!DELOCALIZEDP(an_int));
  MARK_DELOCALIZED(an_int);
  T(DELOCALIZEDP(an_int));
  UNMARK_DELOCALIZED(an_int);
  T(!DELOCALIZEDP(an_int));
  UNMARK_DELOCALIZED(an_int);
  T(!DELOCALIZEDP(an_int));
  MARK_DELOCALIZED(an_int);
  T(DELOCALIZEDP(an_int));
  T(DELOCALIZEDP(an_int));
  T(((uintptr_t)DELOCALIZED(NIL))        == 0xC0FFEEF00DC0FFEE  );
  T(((uintptr_t)DELOCALIZED(TRUE))       == 0xF00DCAFEBAADBEEF  );
  T(((uintptr_t)DELOCALIZED(pool_first)) == 0                    );
  T(((uintptr_t)DELOCALIZED(an_int))     == (((uintptr_t) (an_int) - (uintptr_t)(pool_first)) ));  
  T((LOCALIZED(0xC0FFEEF00DC0FFEE,   pool_first)) == NIL       );
  T((LOCALIZED(0xF00DCAFEBAADBEEF,   pool_first)) == TRUE      );
  T((LOCALIZED(0x0,                   pool_first)) == pool_first);
  
  {
    ae_obj_t * left  = LOCALIZED((ae_obj_t *)0x0, pool_first);
    ae_obj_t * right = (ae_obj_t *)pool_first;
    T(left == right);
    TM("%016p != %016p", left, right);
  }  
  {
    ae_obj_t * left  = DELOCALIZED(LOCALIZED((ae_obj_t *)0x0, pool_first));
    ae_obj_t * right = (ae_obj_t *)0;
    T(left == right);
    TM("%016p != %016p", left, right);
  }
  {
    ae_obj_t * left  = LOCALIZED(DELOCALIZED((ae_obj_t *)pool_first), pool_first);
    ae_obj_t * right = (ae_obj_t *)pool_first;
    T(left == right);
    TM("%016p != %016p", left, right);
  }  
  {
    ae_obj_t * left  = DELOCALIZED(LOCALIZED(an_int, pool_first));
    ae_obj_t * right = an_int;
    T(left == right);
    TM("%016p != %016p", left, right);
  }
  {
    ae_obj_t * left  = LOCALIZED(DELOCALIZED(an_int), pool_first);
    ae_obj_t * right = an_int;
    T(left == right);
    TM("%016p != %016p", left, right);
  }
}
                 
void alist(void) {
  SETUP_TEST;    
                 
  ae_obj_t * list = NIL;
                 
  T(!            AHAS(list, SYM("name")));
  T(CONSP(list = ASET(list, SYM("name"),   NEW_STRING("Bob"))));
  T(             AHAS(list, SYM("name")));
  T(EQL(         AGET(list, SYM("name")),  NEW_STRING("Bob")));
  T(!            AHAS(list, SYM("age")));
  T(CONSP(list = ASET(list, SYM("age"),    NEW_INT(24))));
  T(             AHAS(list, SYM("age")));
  T(EQL(         AGET(list, SYM("age")),   NEW_INT(24)));
  T(CONSP(list = ASET(list, SYM("name"),   NEW_STRING("Jake"))));
  T(! EQL(       AGET(list, SYM("name")),  NEW_STRING("Bob")));
  T(  EQL(       AGET(list, SYM("name")),  NEW_STRING("Jake")));
}                
                 
void plist(void) {
  SETUP_TEST;    
                 
  ae_obj_t * list = NIL;
                 
  T(!            PHAS(list, SYM("name")));
  T(CONSP(list = PSET(list, SYM("name"),   NEW_STRING("Bob"))));
  T(             PHAS(list, SYM("name")));
  T(EQL(         PGET(list, SYM("name")),  NEW_STRING("Bob")));
  T(!            PHAS(list, SYM("age")));
  T(CONSP(list = PSET(list, SYM("age"),    NEW_INT(24))));
  T(             PHAS(list, SYM("age")));
  T(EQL(         PGET(list, SYM("age")),   NEW_INT(24)));
  T(CONSP(list = PSET(list, SYM("name"),   NEW_STRING("Jake"))));
  T(! EQL(       PGET(list, SYM("name")),  NEW_STRING("Bob")));
  T(  EQL(       PGET(list, SYM("name")),  NEW_STRING("Jake")));
}                
                 
void kvp_list(void) {
  SETUP_TEST;    
                 
  ae_obj_t * list = NIL;
                 
  T(!            KHAS(list, SYM("name")));
  T(CONSP(list = KSET(list, SYM("name"),   NEW_STRING("Bob"))));
  T(             KHAS(list, SYM("name")));
  T(EQL(         KGET(list, SYM("name")),  NEW_STRING("Bob")));
  T(!            KHAS(list, SYM("age")));
  T(CONSP(list = KSET(list, SYM("age"),    NEW_INT(24))));
  T(             KHAS(list, SYM("age")));
  T(EQL(         KGET(list, SYM("age")),   NEW_INT(24)));
  T(CONSP(list = KSET(list, SYM("name"),   NEW_STRING("Jake"))));
  T(! EQL(       KGET(list, SYM("name")),  NEW_STRING("Bob")));
  T(  EQL(       KGET(list, SYM("name")),  NEW_STRING("Jake")));
}                

void tailp(void) {
  SETUP_TEST;
  
  T(TAILP(SYM("nil")));
  T(TAILP(CONS(SYM("foo"), NIL)));
  T(TAILP(CONS(NEW_INT(12), CONS(SYM("bar"), NIL))));
  T(TAILP(NEW_CONS(NEW_INT(33), SYM("baz"))));
  T(TAILP(CONS(NIL, NEW_CONS(NEW_INT(33), SYM("baz"))))); 
  T(! TAILP(NEW_INT(22)));
  // T(TAILP(NIL));    // This would generate a -Werror=address error.
  // T(! TAILP(TRUE)); // This would generate a -Werror=address error.
}

/* ae_obj_t * lookup_and_bubble(ae_obj_t * symbols, ae_obj_t * values, ae_obj_t * target) { */
/*   ae_obj_t * symbols_prior = NIL; */
/*   ae_obj_t * values_prior  = NIL; */
/*   ae_obj_t * before_symbols_prior = NIL; */
/*   ae_obj_t * before_values_prior = NIL; */

/*     while (CONSP(symbols) && CONSP(values)) { */
/*         if (CAR(symbols) == target) { */
/*             // Swap in the symbols list */
/*             if (! NILP(before_symbols_prior)) { */
/*                 CDR(before_symbols_prior) = symbols; */
/*             } else { */
/*                 // If the target is the first symbol, adjust the head of the list */
/*                 symbols = symbols_prior; */
/*             } */
/*             CDR(symbols_prior) = CDR(symbols); */
/*             CDR(symbols) = symbols_prior; */

/*             // Swap in the values list */
/*             if (! NILP(before_values_prior)) { */
/*                 CDR(before_values_prior) = values; */
/*             } else { */
/*                 // If the target corresponds to the first value, adjust the head of the list */
/*                 values = values_prior; */
/*             } */
/*             CDR(values_prior) = CDR(values); */
/*             CDR(values) = values_prior; */

/*             // Return the value corresponding to the target symbol after the swap */
/*             return CAR(values); */
/*         } */

/*         before_symbols_prior = symbols_prior; */
/*         symbols_prior = symbols; */
/*         symbols = CDR(symbols); */

/*         before_values_prior = values_prior; */
/*         values_prior = values; */
/*         values = CDR(values); */
/*     } */

/*     // Target symbol not found */
/*     return NIL; */
/* } */

/* void bubble_list(void) { */
/*   SETUP_TEST; */

/*   ae_obj_t * symbols = CONS(SYM("foo"), */
/*                   CONS(SYM("bar"), */
/*                        CONS(SYM("baz"), */
/*                             CONS(SYM("quux"), */
/*                                  CONS(SYM("corge"), NIL))))); */
/*   ae_obj_t * values = CONS(NEW_INT(1), */
/*                  CONS(NEW_INT(2), */
/*                       CONS(NEW_INT(3), */
/*                            CONS(NEW_INT(4), */
/*                                 CONS(NEW_INT(5), NIL))))); */

/*   ae_obj_t * ret = lookup_and_bubble(symbols, values, SYM("baz")); */

/*   LOG(ret, "got;"); */
/*   LOG(symbols , "symbols:"); */
/*   LOG(values, "values:"); */
/*   } */


void env_with_a_dot(void) {
  {
    SETUP_TEST;
  
    ae_obj_t * root   = ENV_NEW_ROOT();
    ae_obj_t * syms   = CONS(SYM("first"), NEW_CONS(SYM("second"), SYM("third")));
    ae_obj_t * values = CONS(NEW_INT(1),
                             CONS(NEW_INT(2),
                                  CONS(NEW_INT(3),
                                       CONS(NEW_INT(4),
                                            CONS(NEW_INT(5), NIL)))));
    ae_obj_t * env    = NEW_ENV(root, syms, values );

    // OLOG(env);
    /* LOG(ENV_SYMS(env), "with syms"); */
    /* LOG(ENV_VALS(env), "and  vals"); */
    
    ae_obj_t * found = ENV_FIND(env, SYM("third"));

    // OLOG(found);

    T(ENV_BOUNDP(env, SYM("third")));
    
    T(LENGTH(found) == 3);

    // OLOG(CAR(found)); 
    T(EQL(CAR(found), NEW_INT(3)));

    // OLOG(CADR(found)); 
    T(EQL(CADR(found), NEW_INT(4)));

    // OLOG(CADDR(found)); 
    T(EQL(CADDR(found), NEW_INT(5)));

    // OLOG(CDDDR(found)); 
    T(NILP(CDDDR(found)));  
  }
  {
    SETUP_TEST;
  
    ae_obj_t * root   = ENV_NEW_ROOT();
    ae_obj_t * syms   = SYM("args");
    ae_obj_t * values = CONS(NEW_INT(3),
                             CONS(NEW_INT(4),
                                  CONS(NEW_INT(5), NIL)));

    ae_obj_t * env    = NEW_ENV(root, syms, values );

    // OLOG(env);
    // LOG(ENV_SYMS(env), "with syms");
    // LOG(ENV_VALS(env), "and  vals");
  
    ae_obj_t * found = ENV_FIND(env, SYM("args"));

    // OLOG(found);

    T(LENGTH(found) == 3);

    // OLOG(CAR(found)); 
    T(EQL(CAR(found), NEW_INT(3)));

    // OLOG(CADR(found)); 
    T(EQL(CADR(found), NEW_INT(4)));

    // OLOG(CADDR(found)); 
    T(EQL(CADDR(found), NEW_INT(5)));

    // OLOG(CDDDR(found)); 
    T(NILP(CDDDR(found)));
  }
  // NL;
}

void eval_args_test(void) {
  SETUP_TEST;

  ae_obj_t * env = ENV_NEW_ROOT();

  ENV_SET(env, SYM("foo"), NEW_INT(10));
  ENV_SET(env, SYM("bar"), NEW_INT(20));
  ENV_SET(env, SYM("baz"), NEW_INT(30));
  
  ae_obj_t * mul_expr = CONS(SYM("*"), CONS(NEW_INT(3), CONS(NEW_INT(4), NIL)));

  /* log_eval = true; */
  /* log_core = true; */
  
  {
    ae_obj_t * args = CONS(NEW_INT(8), CONS(mul_expr, CONS(SYM("foo"), CONS(SYM("bar"), NIL))));
    OLOG(args);
    ae_obj_t * evaled_args = EVAL_ARGS(env, args);
    OLOG(evaled_args);
  }
  {
    ae_obj_t * args = CONS(NEW_INT(8), CONS(mul_expr, CONS(SYM("foo"), NEW_CONS(SYM("bar"), SYM("baz")))));
    OLOG(args);
    ae_obj_t * evaled_args = EVAL_ARGS(env, args);
    OLOG(evaled_args);
  }
  
  NL;
}
  
////////////////////////////////////////////////////////////////////////////////////////////////////
// TEST_LIST
////////////////////////////////////////////////////////////////////////////////////////////////////

#define FOR_EACH_DISABLED_TEST_FUN(DO)                                                             \

#define FOR_EACH_TEST_FUN(DO)                                                                      \
  DO(tailp)                                                                                        \
  DO(test_setup_is_okay)                                                                           \
  DO(newly_allocated_ae_obj_is_inside_pool)                                                        \
  DO(newly_allocated_ae_obj_type_is_AE_INVALID)                                                    \
  DO(newly_initialized_ae_obj_has_correct_type_field)                                              \
  DO(newly_initialized_ae_obj_has_zeroed_data_fields)                                              \
  DO(unsafe_move_an_ae_obj)                                                                        \
  DO(clone_a_simple_ae_obj)                                                                        \
  DO(consed_list_tests)                                                                            \
  DO(pushed_list_tests)                                                                            \
  DO(pushed_and_consed_lists_princ_identically)                                                    \
  DO(improper_list)                                                                                \
  DO(intern_symbols)                                                                               \
  DO(remove_interned_symbol_from_list)                                                             \
  DO(truth)                                                                                        \
  DO(eql)                                                                                          \
  DO(env_basics)                                                                                   \
  DO(env_scoping)                                                                                  \
  DO(fprinc_fwrite_lengths)                                                                        \
  DO(core_cons_car_cdr)                                                                            \
  DO(core_eq_eql_not)                                                                              \
  DO(core_print_princ_write)                                                                       \
  DO(core_math)                                                                                    \
  DO(core_cmp)                                                                                     \
  DO(core_sleep)                                                                                   \
  DO(list_fun)                                                                                     \
  DO(macro_expand)                                                                                 \
  DO(deloc)                                                                                        \
  DO(alist)                                                                                        \
  DO(plist)                                                                                        \
  DO(kvp_list)                                                                                     \
  DO(root_env_and_eval)                                                                            \
  DO(fun_specialness)                                                                              \
  DO(env_with_a_dot)                                                                               \
  DO(eval_args_test)
// DO(bubble_list) 

#define pair(fun) { #fun, fun },

TEST_LIST = {
  FOR_EACH_TEST_FUN(pair)
  { NULL, NULL }
};
