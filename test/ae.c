#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define BEFORE_ACUTEST

#include "ae_obj.h"
#include "ae_list.h"
#include "ae_alist.h"
#include "ae_free_list.h"
#include "ae_core.h"
#include "ae_eval.h"
#include "ae_write.h"
#include "ae_env.h"
#include "ae_util.h"
#include "ae_generate_macro.h"

#include "acutest.h"

#define free_list_size (1 << 12)

#define obj ae_obj_t *

static char mem[free_list_size] = { 0 };

////////////////////////////////////////////////////////////////////////////////////////////////////
// Macros
////////////////////////////////////////////////////////////////////////////////////////////////////

#define T                    TEST_CHECK
#define TM                   TEST_MSG
#define SPC                  (putchar(' '))
#define NL                   (putchar('\n'))
#define FF                   (fflush(stdout))

#define PR(...)              (fprintf(stdout, __VA_ARGS__))
#define COUNT_LIST_LENGTH(l) list_length_counter = 0; EACH((l), incr_list_length_counter)
#define SETQ(env, sym, val)  (ae_core_setq(env, CONS(sym, LIST(val))))

#define SETUP_TEST                                                                                 \
  obj    this    = NULL;                                                                           \
  obj    that    = NULL;                                                                           \
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
  PR("\n\n[describe %s " #fun  "] ", GET_TYPE_STR(fun));                                               \
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
}

char * princ_to_new_string(const obj const this) {
  char * buff;
  size_t size;
  FILE * stream = open_memstream(&buff, &size);

  FPRINC(this, stream);
  fclose(stream);

  return buff;
}

bool shitty_princ_based_equality_predicate(
  const obj const this,
  const char * const strcmp_str) {
  return ! strcmp(strcmp_str, SPRINC(this));
}

obj push_together_a_list_of_ints(void) {
  obj num     = NEW_INT(1);
  obj list    = LIST(num);
  obj tailtip = list;

  T(EQ(LENGTH(tailtip), 1));

  for (int ix = 2; ix < 5; ix++) {
    int        int_val = ix;
    obj new_int = NEW_INT(int_val);

    tailtip            = PUSH(tailtip, new_int);

    T(CONSP(tailtip));
    T(INTEGERP(CAR(tailtip)));
    T(EQ(INT_VAL(CAR(tailtip)), int_val));
    T(EQ(LENGTH(list), ix));
    TM("Length is %zu.", LENGTH(tailtip));
  }

  return list;
}

obj cons_together_a_list_of_ints(void) {
  obj head = NEW_INT(4);
  obj list = LIST(head);

  T(EQ(LENGTH(list), 1));

  for (unsigned int ix  = 0; ix < 3; ix++) {
    obj new_head = NEW_INT(3 - ix);
    obj tail     = list;

    list = CONS(new_head, tail);

    const int expected_length = 2 + ix;

    T(NEQ(list, head));
    T(NEQ(list, new_head));
    T(EQ(CAR(list), new_head));
    T(EQ(CDR(list), tail));
    T(EQ(LENGTH(list), expected_length));
    TEST_MSG(
      "Incorrect length %d, expected %d.",
      LENGTH(list),
      expected_length);
  }

  return list;
}

void incr_list_length_counter(obj const this) {
  (void)this;
  list_length_counter++;
}

obj ae_obj_double(obj const this) {
  assert(INTEGERP(this));

  return NEW_INT(this->int_val * 2);
}

obj ae_obj_to_pairs(obj const this) {
  return CONS(this, LIST(this));
}

void basic_list_checks(obj this) {
  T(EQ(LENGTH(this)       , 4));

  COUNT_LIST_LENGTH(this);
  T(EQ(list_length_counter, 4));
  T(EQ(list_length_counter, LENGTH(this)));

  T(shitty_princ_based_equality_predicate(this, "(1 2 3 4)"));

  obj mapped = NULL;

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
    T(EQ(SYM(str), SYM(str)));                                                                     \
    T(EQ(LENGTH(symbols_list), (len + 1)));                                                        \
  }

  // using 'symbols_list' as the symbol list here:
  T(EQ(SYM("a"), SYM("a")));
  T(EQ(LENGTH(symbols_list), 1));
  T(EQ(SYM("b"), SYM("b")));
  T(EQ(LENGTH(symbols_list), 2));
  T(EQ(SYM("c"), SYM("c")));
  T(EQ(LENGTH(symbols_list), 3));
  T(EQ(SYM("d"), SYM("d")));
  T(EQ(LENGTH(symbols_list), 4));

  that = SYM("b");

  // Add a duplicate element so that we can see that both instances are removed:
  symbols_list = CONS(that, symbols_list);

  T(EQ(LENGTH(symbols_list), 5));
  T(MEMBERP(symbols_list, that));

  symbols_list = REMOVE(symbols_list, that);

  T(EQ(LENGTH(symbols_list), 3));
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
    T(EQ(GET_TYPE(this), _type));                                                                  \
  }
  FOR_EACH_LEXED_TYPE(test);
}

void newly_initialized_ae_obj_has_zeroed_data_fields(void) {
  SETUP_TEST;
  this = NEW(AE_ENV);
  T(NULLP(ENV_PARENT(this)));
  T(NULLP(ENV_SYMS  (this)));
  T(NULLP(ENV_VALS  (this)));
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
  T(EQ(this->numerator_val  , 123));
  T(EQ(this->denominator_val, 321));

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
  T(EQ(that->numerator_val  , 123));
  T(EQ(that->denominator_val, 321));
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
  T(EQ(SYM("one"), SYM("one")));
  T(EQ(LENGTH(symbols_list), 1));

  T(! strcmp(SYM_VAL(SYM("two")), "two"));
  T(NEQ(SYM("one"), SYM("two")));
  T(EQ(LENGTH(symbols_list), 2));
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
    T(EQ(strlen(buff), expect));                                                                   \
    T(EQ((int)strlen(buff), (int)size));                                                           \
    TM("strlen was %d but size was %d:\n\"%s\".\n",                                                \
       (int)strlen(buff), (int)size, buff);                                                        \
    T(EQ((int)strlen(buff), (int)reported));                                                       \
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

  obj obj_int_2a      = NEW_INT(2);
  obj obj_int_2b      = NEW_INT(2);
  obj obj_float_2a    = NEW_FLOAT(2.0);
  obj obj_float_2b    = NEW_FLOAT(2.0);
  obj obj_int_3a      = NEW_INT(3);
  obj obj_int_3b      = NEW_INT(3);
  obj obj_float_3a    = NEW_FLOAT(3.0);
  obj obj_float_3b    = NEW_FLOAT(3.0);
  obj obj_bool_false  = TRUTH(false);
  obj obj_bool_true   = TRUTH(obj_float_2a);
  obj obj_list_consed = cons_together_a_list_of_ints();
  obj obj_list_pushed = push_together_a_list_of_ints();
  obj obj_rat_2a      = NEW_RATIONAL(2, 1);
  obj obj_rat_2b      = NEW_RATIONAL(4, 2);
  obj obj_rat_3a      = NEW_RATIONAL(3, 1);
  obj obj_rat_3b      = NEW_RATIONAL(9, 3);
  obj obj_char_a_a    = NEW_CHAR('a');
  obj obj_char_a_b    = NEW_CHAR('a');
  obj obj_char_b_a    = NEW_CHAR('b');
  obj obj_char_b_b    = NEW_CHAR('b');
  char     * pchar_a         = "a";
  obj obj_string_a_a  = NEW_STRING(pchar_a);
  obj obj_string_a_b  = NEW_STRING(pchar_a);
  obj obj_string_a_c  = NEW_STRING("a");
  obj obj_string_b_a  = NEW_STRING("b");

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
    T( NEQL( (first)  , (second) ));                                                               \
    T( NEQL( (second) , (first)  ));                                                               \
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

void envs(void) {
  SETUP_TEST;

  this = NEW_ENV(NIL, NIL, NIL);

  T(LENGTH(ENV_SYMS(this)) == 0);
  T(LENGTH(ENV_VALS(this)) == 0);
  T(NILP(ENV_SYMS(this)));
  T(NILP(ENV_VALS(this)));

  T(! MEMBERP(ENV_SYMS(this), SYM("foo")));

  ENV_ADD(this, SYM("foo"), NEW_INT(12));

  T(EQ(LENGTH(ENV_SYMS(this)), 1));
  T(EQ(LENGTH(ENV_VALS(this)), 1));
  T(MEMBERP(ENV_SYMS(this), SYM("foo")));
  T(! NILP(ENV_SYMS(this)));
  T(! NILP(ENV_VALS(this)));

  T(! MEMBERP(ENV_SYMS(this), SYM("bar")));

  ENV_ADD(this, SYM("bar"), NEW_INT(24));

  T(EQ(LENGTH(ENV_SYMS(this)), 2));
  T(EQ(LENGTH(ENV_VALS(this)), 2));
  T(MEMBERP(ENV_SYMS(this), SYM("bar")));

  T(! MEMBERP(ENV_SYMS(this), SYM("baz")));

  ENV_ADD(this, SYM("baz"), NEW_INT(36));

  T(EQ(LENGTH(ENV_SYMS(this)), 3));
  T(EQ(LENGTH(ENV_VALS(this)), 3));
  T(MEMBERP(ENV_SYMS(this), SYM("baz")));
  T(EQ(INT_VAL(ENV_FIND(this, SYM("foo"))), 12));
  T(EQ(INT_VAL(ENV_FIND(this, SYM("bar"))), 24));
  T(EQ(INT_VAL(ENV_FIND(this, SYM("baz"))), 36));

  that = NEW_ENV(NIL, NIL, NIL); // not yet linked to.

  ENV_ADD(that, SYM("quux"), NEW_INT(48));

  T(NILP(ENV_FIND(this, SYM("quux"))));

  ENV_PARENT(this) = that; // link this to that.

  T(EQ(INT_VAL(ENV_FIND(this, SYM("quux"))), 48));
  T(EQ(ENV_FIND(this, SYM("quux")), ENV_FIND(that, SYM("quux"))));
  T(NILP(ENV_FIND(this, SYM("zot"))));
  T(NILP(ENV_FIND(that, SYM("foo"))));

  ENV_SET(this, SYM("bar"), NEW_INT(99));

  T(EQ(INT_VAL(ENV_FIND(this, SYM("bar"))), 99));

  ENV_SET(this, SYM("zot"), NEW_INT(66));

  T(EQ(INT_VAL(ENV_FIND(this, SYM("zot"))), 66));

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
  T(EQ(LENGTH(this)       , -1));
  TM("Expected length 3, got %d.", LENGTH(this));

  COUNT_LIST_LENGTH(this);
  T(EQ(list_length_counter, 3));

  T(shitty_princ_based_equality_predicate(this, "(1 2 3 . 4)"));

  obj mapped = MAP(this, ae_obj_double);
  T(shitty_princ_based_equality_predicate(mapped, "nil"));
  T(NILP(mapped));

  // PUT(NEW_CONS(NEW_INT(1), NEW_INT(2)));
}

obj make_args_containing_one_list(void) {
  return LIST(CONS(SYM("a"), CONS(SYM("b"), LIST(SYM("c")))));
}

obj make_args_for_cons(void) {
  return CONS(NIL, LIST(CONS(SYM("a"), CONS(SYM("b"), LIST(SYM("c"))))));
}

void core_cons_car_cdr(void) {
  SETUP_TEST;
  obj env   = ENV_NEW_ROOT();

  T(EQ(ae_core_car(env, make_args_containing_one_list()), SYM("a")                                           ));
  T(EQ(ae_core_car(env, LIST(ae_core_cdr(env, make_args_containing_one_list()))), SYM("b")                   ));
  T(shitty_princ_based_equality_predicate(ae_core_cons(env, CONS(SYM("a"), LIST(NIL))), "(a)"                )); // cons 'a onto nil and get (a).
  T(shitty_princ_based_equality_predicate(ae_core_cdr (env, make_args_containing_one_list() ), "(b c)"       ));
  T(shitty_princ_based_equality_predicate(ae_core_cons(env, make_args_for_cons()            ), "(nil a b c)" ));
  T(NILP(ae_core_car(env,                  LIST(NIL))                                                        ));
  T(NILP(ae_core_cdr(env,                  LIST(NIL))                                                        ));
  T(NILP(ae_core_car(env, LIST(ae_core_car(env, LIST(NIL))))                                                 ));
  T(NILP(ae_core_cdr(env, LIST(ae_core_cdr(env, LIST(NIL))))                                                 ));
  T(NILP(ae_core_car(env, LIST(ae_core_cdr(env, LIST(NIL))))                                                 ));
  T(NILP(ae_core_cdr(env, LIST(ae_core_car(env, LIST(NIL))))                                                 ));
}

void core_eq_eql_not(void) {
  SETUP_TEST;
  obj env   = ENV_NEW_ROOT();

  this = CONS(NEW_INT(1), LIST(NEW_INT(2)));
  that = CONS(NEW_INT(1), LIST(NEW_INT(2)));

  T(TRUEP(ae_core_eql  (env, CONS(NEW_INT(5), LIST(NEW_INT  (5  ))                           )))); // 5 and 5 are equal numbers...
  T(NILP (ae_core_eq   (env, CONS(NEW_INT(5), LIST(NEW_INT  (5  ))                           )))); // ... but they are not the same object.
  T(TRUEP(ae_core_eql  (env, CONS(NEW_INT(5), LIST(NEW_FLOAT(5.0))                           )))); // 5 and 5.0 are equal-enough numbers...
  T(NILP (ae_core_eql  (env, CONS(NEW_INT(5), LIST(NEW_INT  (6  ))                           )))); // ... but 5 and 6 are not.

  T(TRUEP(ae_core_eq   (env, CONS(this      , LIST(this)                                     )))); // These are the same object and so are eq
  T(TRUEP(ae_core_eql  (env, CONS(this      , LIST(this)                                     )))); // and also eql.
  T(NILP (ae_core_eq   (env, CONS(this      , LIST(that)                                     )))); // These are the NOT the same object and so
  T(NILP (ae_core_eql  (env, CONS(this      , LIST(that)                                     )))); // neither eq or eql.
  T(NILP (ae_core_eq   (env, CONS(that      , LIST(this)                                     )))); // eq is commutative.
  T(NILP (ae_core_eql  (env, CONS(that      , LIST(this)                                     )))); // eql too.

  T(TRUEP(ae_core_eq   (env, CONS(this      , CONS(this         , LIST(this                 )))))); // eq can take 3+ arguments...
  T(NILP (ae_core_eq   (env, CONS(this      , CONS(this         , LIST(that                 ))))));
  T(TRUEP(ae_core_eq   (env, CONS(NIL       , CONS(NIL          , LIST(NIL                  ))))));
  T(NILP (ae_core_eq   (env, CONS(NIL       , CONS(NIL          , LIST(TRUE                 ))))));

  T(TRUEP(ae_core_eql  (env, CONS(NEW_INT(5), CONS(NEW_INT(5)   , LIST(NEW_INT(5)           )))))); // ...so can eql.
  T(NILP (ae_core_eql  (env, CONS(NEW_INT(5), CONS(NEW_INT(5)   , LIST(NEW_INT(6)           ))))));

  T(TRUEP(ae_core_not  (env, CONS(NIL       , CONS(NIL          , LIST(NIL                  ))))));
  T(NILP (ae_core_not  (env, CONS(NIL       , CONS(NIL          , LIST(TRUE                 ))))));
}

void core_print_princ_write(void) {
  SETUP_TEST;
  obj env   = ENV_NEW_ROOT();

  NL;
  {
    PR("write-ing '\"hello\" 5 a abc' on the next line, with quoting: ");
    NL;

    obj written  = ae_core_write(env,
      CONS(NEW_STRING("hello"),
           CONS(NEW_INT(5),
           CONS(NEW_CHAR('a'),
           CONS(SYM("abc"),
           NIL)))));
    NL;
    T(INT_VAL(written) == 17);
    TM("Expected %d, wrote %d.", 17, INT_VAL(written));
  }
  {
    PR("\nprint-ing \"hello\",  5 a, abc on the next 3 lines, with quoting: ");
    obj written = ae_core_print(env,
      CONS(NEW_STRING("hello"),
           CONS(NEW_INT(5),
           CONS(NEW_CHAR('a'),
           CONS(SYM("abc"),
           NIL)))));
    NL;
    T(INT_VAL(written) == 21);
    TM("Expected %d, wrote %d.", 14, INT_VAL(written));
  }
  {
    PR("\nprinc-ing 'hello5aabc' on the next line, without quoting: ");
    NL;
    obj written = ae_core_princ(env,
      CONS(NEW_STRING("hello"),
           CONS(NEW_INT(5),
           CONS(NEW_CHAR('a'),
           CONS(SYM("abc"),
           NIL)))));;
    NL;
    T(INT_VAL(written) == 10);
    TM("Expected %d, wrote %d.", 10, INT_VAL(written));
  }
  NL;
}

void core_math(void) {
  SETUP_TEST;
  obj env   = ENV_NEW_ROOT();

  this = ae_core_add(env, CONS(NEW_INT(24), CONS(NEW_INT(4), LIST(NEW_INT(3) ))));
  T(EQL(this, NEW_INT(31)));

  this = ae_core_sub(env, CONS(NEW_INT(24), CONS(NEW_INT(4), LIST(NEW_INT(3) ))));
  T(EQL(this, NEW_INT(17)));

  this = ae_core_sub(env, CONS(NEW_INT(3),  CONS(NEW_INT(4), LIST(NEW_INT(24)))));
  T(EQL(this, NEW_INT(-25)));

  this = ae_core_mul(env, CONS(NEW_INT(24), CONS(NEW_INT(4), LIST(NEW_INT(3) ))));
  T(EQL(this, NEW_INT(288)));

  this = ae_core_div(env, CONS(NEW_INT(24), CONS(NEW_INT(4), LIST(NEW_INT(3) ))));
  T(EQL(this, NEW_INT(2)));
}

void core_cmp(void) {
  SETUP_TEST;
  obj env   = ENV_NEW_ROOT();

  T(TRUEP(ae_core_equal (env, CONS(NEW_INT(2), CONS(NEW_INT(2), LIST(NEW_INT(2)))))));
  T(NILP (ae_core_equal (env, CONS(NEW_INT(2), CONS(NEW_INT(2), LIST(NEW_INT(3)))))));

  T(NILP (ae_core_nequal(env, CONS(NEW_INT(2), CONS(NEW_INT(2), LIST(NEW_INT(2)))))));
  T(TRUEP(ae_core_nequal(env, CONS(NEW_INT(2), CONS(NEW_INT(2), LIST(NEW_INT(3)))))));

  T(TRUEP(ae_core_lt (env, CONS(NEW_INT(2), CONS(NEW_INT(4), LIST(NEW_INT(6)))))));
  T(NILP (ae_core_gt (env, CONS(NEW_INT(2), CONS(NEW_INT(4), LIST(NEW_INT(6)))))));

  T(TRUEP(ae_core_gt (env, CONS(NEW_INT(6), CONS(NEW_INT(4), LIST(NEW_INT(2)))))));
  T(NILP (ae_core_lt (env, CONS(NEW_INT(6), CONS(NEW_INT(4), LIST(NEW_INT(2)))))));

  T(TRUEP(ae_core_lte(env, CONS(NEW_INT(2), CONS(NEW_INT(4), CONS(NEW_INT(4), LIST(NEW_INT(6))))))));
  T(NILP (ae_core_gte(env, CONS(NEW_INT(2), CONS(NEW_INT(4), CONS(NEW_INT(4), LIST(NEW_INT(6))))))));

  T(TRUEP(ae_core_gte(env, CONS(NEW_INT(6), CONS(NEW_INT(4), CONS(NEW_INT(4), LIST(NEW_INT(2))))))));
  T(NILP (ae_core_lte(env, CONS(NEW_INT(6), CONS(NEW_INT(4), CONS(NEW_INT(4), LIST(NEW_INT(2))))))));
}

void core_msleep(void) {
  SETUP_TEST;
  obj env   = ENV_NEW_ROOT();

  obj expr = NIL;

  ae_env_define_list_and_quote(env);

  obj add   = CONS(CONS(SYM("+"), CONS(SYM("xx"), CONS(NEW_INT(2), NIL))), NIL);
  obj incr2 = CONS(SYM("setq"),   CONS(SYM("xx"), add));
  obj print = CONS(SYM("print"),  CONS(SYM("xx"), NIL));
  obj msleep = CONS(SYM("msleep"),  CONS(NEW_INT(100), NIL));

  EVAL(env, CONS(SYM("setq"), CONS(SYM("xx"), CONS(NEW_INT(10), NIL))));

  for (int ix = 0; ix < 6; ix++) {
    expr            = CONS(incr2, expr);
    expr            = CONS(msleep, expr);
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

  obj env    = ENV_NEW_ROOT();
  ae_env_define_list_and_quote(env);

  obj listf  = EVAL(env, SYM("list"));
  OLOG(listf);
  OLOG(FUN_PARAMS(listf));
  OLOG(FUN_BODY  (listf));

  obj expr   = NIL;
  obj rtrn   = NIL;

  NL;
  PR("syms: "); WRITE(ENV_SYMS(env)); NL;
  PR("vals: "); WRITE(ENV_VALS(env)); NL;

  SETQ(env, SYM("foo"), NEW_INT(666));

  T(EQL(NEW_INT(25),  EVAL(env, CONS(SYM("+"), CONS(NEW_INT(16), LIST(NEW_INT(9)))))));
  T(EQL(NEW_INT(672), EVAL(env, CONS(SYM("+"), CONS(NEW_INT(6), LIST(SYM("foo")))))));
  TM("Expected %d, got %d.", 672, INT_VAL(EVAL(env, CONS(SYM("+"), CONS(NEW_INT(6), LIST(SYM("foo")))))));

  T(EQL(NEW_INT(75),  EVAL(env, CONS(SYM("*"), CONS(NEW_INT(3),  LIST(CONS(SYM("+"), CONS(NEW_INT(16), LIST(NEW_INT(9))))))))));

  EVAL(env, CONS(SYM("setq"), CONS(SYM("bar"), LIST(NEW_INT(9)))));
  EVAL(env, CONS(SYM("setq"), CONS(SYM("baz"), LIST(CONS(SYM("+"), CONS(NEW_INT(16), LIST(NEW_INT(9))))))));

  T(EQL(NEW_INT(9),   EVAL(env, SYM("bar"))));
  T(EQL(NEW_INT(25),  EVAL(env, SYM("baz"))));

  expr = CONS(SYM("progn"), CONS(CONS(SYM("princ"), LIST(NEW_STRING("Hello "))), CONS(CONS(SYM("princ"), LIST(NEW_STRING("from Ash"))), LIST(CONS(SYM("princ"), LIST(NEW_STRING("Lisp!")))))));

  NL;
  PR("Printing \"Hello from Ash Lisp!\" on the next line:\n");
  this = EVAL(env, expr);
  T(EQL(NEW_INT(5), this));
  NL;

  expr = CONS(SYM("quote"), LIST(CONS(NEW_INT(5), CONS(NEW_INT(10), LIST(NEW_INT(15))))));
  T(shitty_princ_based_equality_predicate(EVAL(env, expr), "(5 10 15)"));

  expr = CONS(SYM("quote"), LIST(SYM("a")));
  T(shitty_princ_based_equality_predicate(EVAL(env, expr), "a"));

  expr = CONS(SYM("if"), CONS(SYM("t"), CONS(NEW_INT(11), CONS(SYM("ignored"), LIST(NEW_INT(22))))));
  T(EQL(NEW_INT(11), EVAL(env, expr)));

  expr = CONS(SYM("if"), CONS(SYM("t"), LIST(NEW_INT(11))));
  T(EQL(NEW_INT(11), EVAL(env, expr)));

  expr = CONS(SYM("if"), CONS(SYM("nil"), CONS(NEW_INT(11), CONS(SYM("ignored"), LIST(NEW_INT(22))))));
  T(EQL(NEW_INT(22), EVAL(env, expr)));

  expr = CONS(SYM("if"), CONS(SYM("nil"), LIST(NEW_INT(11))));
  T(NILP(EVAL(env, expr)));

  expr = CONS(SYM("lambda"),
              CONS(LIST(SYM("x")),
                   CONS(
                     CONS(SYM("princ"), LIST(SYM("x"))), LIST(
                       CONS(SYM("+"),
                            CONS(SYM("x"), LIST(NEW_INT(2))))))));

  rtrn = EVAL(env, expr);

  T(LAMBDAP(rtrn));

  obj subexpr = CONS(SYM("*"), CONS(NEW_INT(3), LIST(NEW_INT(5))));

  expr = CONS(CONS(SYM("lambda"),
                   CONS(LIST(SYM("x")),
                        CONS(CONS(SYM("princ"),
                                  LIST(SYM("x"))),
                             LIST(CONS(SYM("+"),
                                       CONS(SYM("x"),
                                            LIST(subexpr))))))),
              LIST(NEW_INT(12)));


  PR("\nPrinting 12 on the next line:\n");
  rtrn = EVAL(env, expr);
  NL;

  // no princ:
  expr = CONS(CONS(SYM("lambda"),
                 CONS(LIST(SYM("x")),
                      LIST(CONS(SYM("+"),
                                CONS(SYM("x"),
                                     LIST(NEW_INT(1))))))),
            LIST(CONS(SYM("+"),
                      CONS(NEW_INT(2),
                           LIST(NEW_INT(3))))));

  PR("\nPrinting 6 on the next line:\n");
  rtrn = EVAL(env, expr);
  WRITE(rtrn);
  NL;

  T(EQL(NEW_INT(6), rtrn));

  obj    expr1 = CONS(CONS(SYM("=="), CONS(SYM("a"), LIST(NEW_INT(1)))), LIST(NEW_INT(10)));
  obj    expr2 = CONS(CONS(SYM("=="), CONS(SYM("a"), LIST(NEW_INT(2)))), LIST(NEW_INT(20)));
  obj    expr3 = CONS(TRUE, LIST(NEW_INT(30)));
  expr                = CONS(SYM("cond"), CONS(expr1, CONS(expr2, LIST(expr3))));

  NL;
  PR("Evaluating this cond: ");
  WRITE(expr);
  PR(".");
  NL;

#define TEST_COND(input, expected)                                                                            \
  {                                                                                                           \
    SETQ(env, SYM("a"), NEW_INT(input));                                                                      \
    this = EVAL(env, expr);                                                                                   \
    PR("Rtrn for " #input " is ");                                                                            \
    PRINC(this);                                                                                              \
    PR(".");                                                                                                  \
    NL;                                                                                                       \
    if (NEQL(this, NEW_INT(expected))) {                                                                      \
      NL;                                                                                                     \
      PR("this ");                                                                                            \
      WRITE(this);                                                                                            \
      PR(" == expected ");                                                                                    \
      WRITE(NEW_INT(expected));                                                                               \
      NL;                                                                                                     \
    }                                                                                                         \
    T(EQL(this, NEW_INT(expected)));                                                                          \
  }

  TEST_COND(1, 10);
  TEST_COND(2, 20);
  TEST_COND(3, 30);

  NL;
}

void list_fun(void) {
  SETUP_TEST;

  obj env           = ENV_NEW_ROOT();

  ae_env_define_list_and_quote(env);

  //obj list_fun      = ae_env_define_list_and_quote(env);
  obj list_fun      = ENV_FIND(env, SYM("list"));
  obj list_fun_call = CONS(list_fun, CONS(NEW_INT(1), CONS(NEW_INT(2), LIST(NEW_INT(3)))));
  obj ret           = EVAL(env, list_fun_call);

  T(shitty_princ_based_equality_predicate(ret, "(1 2 3)"));
  NL;
}

obj apply_user_fun(obj fun, obj env, obj args);

#define GENERATED_MACRO_TEST(name, expect_str)     \
 {                                                 \
   tmp_str = SPRINC(ae_generate_macro_ ## name()); \
                                                   \
   if (strcmp(tmp_str, expect_str)) {              \
     NL;                                           \
     PR("Got      %s\n", tmp_str);                 \
     PR("Wanted   %s\n", expect_str);              \
   }                                               \
                                                   \
   T(! strcmp(tmp_str, expect_str));               \
   free(tmp_str);                                  \
 }

void macro_expand(void) {
  SETUP_TEST;

  PR("\n\nPopulating root env...");
  obj env = ENV_NEW_ROOT();
  ae_env_define_list_and_quote(env);
  PR("\nDone populating root env.\n");

  GENERATED_MACRO_TEST(defmacro, "(setq defmacro (macro (name params . body) (list (quote setq) name (list (quote macro) params . body))))");
  GENERATED_MACRO_TEST(defun,    "(defmacro defun (name params . body) (list (quote setq) name (list (quote lambda) params . body)))");
  GENERATED_MACRO_TEST(and,      "(defmacro and args (cond ((null args) t) ((null (cdr args)) (car args)) (t (list (quote if) (car args) (cons (quote and) (cdr args))))))");
  GENERATED_MACRO_TEST(or,       "(defmacro or args (if (null args) nil (cons (quote cond) (mapcar list args))))");

  obj macro_def = NIL;
  macro_def = CONS(CONS(SYM("quote"), CONS(SYM("+"), NIL)), CONS(SYM("xxx"), CONS(SYM("yyy"), macro_def)));
  macro_def = CONS(CONS(SYM("list"), macro_def), NIL);
  macro_def = CONS(CONS(SYM("xxx"), CONS(SYM("yyy"), NIL)),  macro_def);
  macro_def = CONS(SYM("macro"), macro_def);
  PR("macro def  "); PRINC(macro_def); NL;
  PR("should be  (macro (xxx yyy) (list (quote +) xxx yyy))");
  T(shitty_princ_based_equality_predicate(macro_def, "(macro (xxx yyy) (list (quote +) xxx yyy))"));

  obj setq_for_macro_def   = CONS(SYM("setq"), CONS(SYM("add2"), CONS(macro_def, NIL)));
  obj rtrn_for_macro_def   = EVAL(env, setq_for_macro_def);
  NL;
  OLOG(setq_for_macro_def);
  OLOG(rtrn_for_macro_def);
  NL;

  obj call_add2      = CONS(SYM("add2"), CONS(NEW_INT(5), CONS(NEW_INT(8), NIL)));
  OLOG(call_add2);
  NL;

  obj call_add2_rtrn = EVAL(env, call_add2);
  OLOG(call_add2_rtrn);
  NL;

  obj eval_call_add2_rtrn = EVAL(env, call_add2_rtrn);
  OLOG(eval_call_add2_rtrn);
  NL;

  /* { */
  /*   obj princ = CONS(SYM("princ"),  CONS(CONS(SYM("quote"), CONS(CONS(SYM("hello"), CONS(NEW_STRING("hello"), NIL)), NIL)), NIL)   ); */
  /*   NL; */
  /*   OLOG(princ); */
  /*   NL; */
  /*   obj princed_len = EVAL(env, princ); */
  /*   NL; */
  /*   OLOG(princed_len); */
  /*   NL; */
  /* } */

  /* { */
  /*   obj princ = CONS(SYM("print"),  CONS(CONS(SYM("quote"), CONS(CONS(SYM("hello"), CONS(NEW_STRING("hello"), NIL)), NIL)), NIL)   ); */
  /*   NL; */
  /*   OLOG(princ); */
  /*   NL; */
  /*   obj princed_len = EVAL(env, princ); */
  /*   NL; */
  /*   OLOG(princed_len); */
  /*   NL; */
  /* } */
}

void alist(void) {
  SETUP_TEST;

  ae_obj_t * alist = NIL;

  T(!      AHAS(alist, SYM("name")));

  alist =  ASET(alist, SYM("name"),   NEW_STRING("Bob"));

  T(       AHAS(alist, SYM("name")));
  T(  EQL( AGET(alist, SYM("name")),  NEW_STRING("Bob")));
  T(!      AHAS(alist, SYM("age")));

  alist =  ASET(alist, SYM("age"),    NEW_INT(24));

  T(       AHAS(alist, SYM("age")));
  T( EQL(  AGET(alist, SYM("age")),   NEW_INT(24)));

  alist =  ASET(alist, SYM("name"),   NEW_STRING("Jake"));

  T(!  EQL(AGET(alist, SYM("name")),  NEW_STRING("Bob")));
  T(   EQL(AGET(alist, SYM("name")),  NEW_STRING("Jake")));

  NL;
  OLOG(alist);
  NL;
}

void deloc(void) {
  SETUP_TEST;

  /* for(int ix = 0; ix < 64; ix++) { */
    ae_obj_t * o = NEW_INT(14);

    // PR("Before: "); 
    // ae_put_words(o);
    //NL;
    
    T(! DELOCP(o));

    DELOC(o);

    T(DELOCP(o));

    RELOC(o);

    T(! DELOCP(o));


    RELOC(o);

    T(! DELOCP(o));

    DELOC(o);

    T(DELOCP(o));

    T(DELOCP(o));

    obj car  = TRUE;
    obj cadr = NEW_INT(4);
    obj cons = CONS(NIL, CONS(TRUE, NIL));
    obj cdr  = CDR(cons); 
    
    PUT(cons);
    NL;
    
    PR("cons       = 0x%016" PRIX64 " \n",      (uintptr_t)             cons );
    PR("cons' type = %s \n",         (uintptr_t)GET_TYPE_STR(cons));
    PR("cons' car  = 0x%016" PRIX64 " \n",      (uintptr_t)CAR         (cons));
    PR("cons' cdr  = 0x%016" PRIX64 " \n\n",    (uintptr_t)CDR         (cons));
    
    PR("car        = 0x%016" PRIX64 " \n",      (uintptr_t)             car  );
    PR("car'  type = %s \n",         (uintptr_t)GET_TYPE_STR(car ));
    if (CONSP(car)) {
      PR("car' car   = 0x%016" PRIX64 " \n",    (uintptr_t)CAR         (car ));
      PR("car' cdr   = 0x%016" PRIX64 " \n\n",  (uintptr_t)CDR         (car ));
    }
    NL;
    
    PR("cdr        = 0x%016" PRIX64 " \n",                              cdr  );
    PR("cdr'  type = %s \n",         (uintptr_t)GET_TYPE_STR(cdr ));
    if (CONSP(cdr)) {
      PR("cdr'  car  = 0x%016" PRIX64 " \n",    (uintptr_t)CAR         (cdr ));
      PR("cdr'  cdr  = 0x%016" PRIX64 " \n\n",  (uintptr_t)CDR         (cdr ));
    }

    PR("size of ae_obj_t * = %16d \n\n",                     sizeof(ae_obj_t *));
    PR("size of uintptr_t  = %16d \n\n",                     sizeof(uintptr_t));
    PR("pool is at         = 0x%016" PRIX64 " \n",           (uintptr_t)pool_first);
    PR("nil is at          = 0x%016" PRIX64 " \n",           (uintptr_t)NIL);
    PR("true is at         = 0x%016" PRIX64 " \n",           (uintptr_t)TRUE);
    
    NL;
    
    PR("cons' cdr  = 0x%016" PRIX64 " \n",                   (uintptr_t)CDR (cons));
    DELOCALIZE(CAR(cons));
    DELOCALIZE(CDR(cons));
    PR("cons' cdr  = 0x%016" PRIX64 " \n\n",                 (uintptr_t)CDR (cons));

    PR("DELOCALIZED(pool_first) is at = 0x%016" PRIX64 " \n", ((uintptr_t)(DELOCALIZED(pool_first))));
    
    NL;NL;NL;
    
    DELOCALIZE(CAR(cdr ));
    DELOCALIZE(CDR(cdr ));

    PR("cons       = 0x016%" PRIX64 " \n",      (uintptr_t)             cons );
    PR("cons' type = %s \n",                    (uintptr_t)GET_TYPE_STR(cons));
    PR("cons' car  = 0x%016" PRIX64 " \n",      (uintptr_t)CAR         (cons));
    PR("cons' cdr  = 0x%016" PRIX64 " \n\n",    (uintptr_t)CDR         (cons));
    
    PR("car        = 0x%016" PRIX64 " \n",      (uintptr_t)             car  );
    PR("car'  type = %s \n",                    (uintptr_t)GET_TYPE_STR(car ));
    if (CONSP(car)) {
      PR("car' car   = 0x%016" PRIX64 " \n",    (uintptr_t)CAR         (car ));
      PR("car' cdr   = 0x%016" PRIX64 " \n\n",  (uintptr_t)CDR         (car ));
    }
    NL;
    
    PR("cdr        = 0x%016" PRIX64 " \n",                              cdr  );
    PR("cdr'  type = %s \n",                    (uintptr_t)GET_TYPE_STR(cdr ));
    if (CONSP(cdr)) {
      PR("cdr'  car  = 0x%016" PRIX64 " \n",    (uintptr_t)CAR         (cdr ));
      PR("cdr'  cdr  = 0x%016" PRIX64 " \n\n",  (uintptr_t)CDR         (cdr ));
    }

    obj an_int = NEW_INT(1);
    
    T(((uintptr_t)DELOCALIZED(NIL))        == 0xC0FFEEF00DC0FFEE  );
    T(((uintptr_t)DELOCALIZED(TRUE))       == 0xF00DCAFEBAADBEEF  );
    T(((uintptr_t)DELOCALIZED(pool_first)) == 0                   );
    T(((uintptr_t)DELOCALIZED(pool_first)) == ( (uintptr_t)     0));
    T(((uintptr_t)DELOCALIZED(an_int))     == (((uintptr_t) (an_int) - (uintptr_t)(pool_first)) ));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// TEST_LIST
////////////////////////////////////////////////////////////////////////////////////////////////////

#define FOR_EACH_DISABLED_TEST_FUN(DO)                                                             \

#define FOR_EACH_TEST_FUN(DO)                                                                      \
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
  DO(alist)                                                                                        \
  DO(envs)                                                                                         \
  DO(fprinc_fwrite_lengths)                                                                        \
  DO(core_cons_car_cdr)                                                                            \
  DO(core_eq_eql_not)                                                                              \
  DO(core_print_princ_write)                                                                       \
  DO(core_math)                                                                                    \
  DO(core_cmp)                                                                                     \
  DO(core_msleep)                                                                                  \
  DO(list_fun)                                                                                     \
  DO(macro_expand)                                                                                 \
  DO(root_env_and_eval)                                                                            \
  DO(deloc)                                                                                        

#define pair(fun) { #fun, fun },

TEST_LIST = {
  FOR_EACH_TEST_FUN(pair)
  { NULL, NULL }
};
