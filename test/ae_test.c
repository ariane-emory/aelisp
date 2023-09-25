#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BEFORE_ACUTEST

#include "ae_obj.h"
#include "ae_free_list.h"
#include "ae_core.h"
#include "ae_eval.h"
#include "ae_write.h"
#include "ae_env.h"
#include "ae_util.h"
#include "ae_generate_macro.h"

#include "acutest.h"

#define free_list_size (1 << 12)

#undef DOT
#define DOT NEW_CONS

static char mem[free_list_size] = { 0 };

////////////////////////////////////////////////////////////////////////////////////////////////////
// Macros
////////////////////////////////////////////////////////////////////////////////////////////////////

#define T       TEST_CHECK
#define TM      TEST_MSG
#define SPC     (putchar(' '))
#define NL      (putchar('\n'))
#define FF      (fflush(stdout))
#define PR(...) (fprintf(stdout, __VA_ARGS__))

#define COUNT_LIST_LENGTH(l) list_length_counter = 0; EACH((l), incr_list_length_counter);

static char * tmp_str = NULL;

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

////////////////////////////////////////////////////////////////////////////////////////////////////
// Helpers
////////////////////////////////////////////////////////////////////////////////////////////////////

void before_acutest() {}

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
  ae_obj_t * list    = LIST(num);
  ae_obj_t * tailtip = list;

  T(EQ(LENGTH(tailtip), 1));

  for (int ix = 2; ix < 5; ix++) { 
    int        int_val = ix;
    ae_obj_t * new_int = NEW_INT(int_val);

    tailtip            = PUSH(tailtip, new_int);
    
    T(CONSP(tailtip));
    T(INTEGERP(CAR(tailtip)));
    T(EQ(INT_VAL(CAR(tailtip)), int_val));
    T(EQ(LENGTH(list), ix));
    TM("Length is %zu.", LENGTH(tailtip));
  }

  return list;
}

ae_obj_t * cons_together_a_list_of_ints(void) {
  ae_obj_t * head = NEW_INT(4);
  ae_obj_t * list = LIST(head);
  
  T(EQ(LENGTH(list), 1));

  for (unsigned int ix  = 0; ix < 3; ix++) { 
    ae_obj_t * new_head = NEW_INT(3 - ix);
    ae_obj_t * tail     = list;
    
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

static int list_length_counter = 0;

void incr_list_length_counter(ae_obj_t * const this) {
  (void)this;
  list_length_counter++;
}

ae_obj_t * ae_obj_double(ae_obj_t * const this) {
  ASSERT_INTEGERP(this);

  return NEW_INT(this->int_val * 2);
}

ae_obj_t * ae_obj_to_pairs(ae_obj_t * const this) {
  return CONS(this, LIST(this));
}

void basic_list_checks(ae_obj_t * this) {
  T(EQ(LENGTH(this)       , 4));
  
  COUNT_LIST_LENGTH(this);
  T(EQ(list_length_counter, 4));
  T(EQ(list_length_counter, LENGTH(this)));

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
 
#define TEST_INTERN(str)                                                                                   \
  {                                                                                                        \
    int len = LENGTH(symbols_list);                                                                        \
    T(EQ(INTERN(str), INTERN(str)));                                                                       \
    T(EQ(LENGTH(symbols_list), (len + 1)));                                                                \
  }
   
  // using 'symbols_list' as the symbol list here:  
  T(EQ(INTERN("a"), INTERN("a")));
  T(EQ(LENGTH(symbols_list), 1));
  T(EQ(INTERN("b"), INTERN("b")));
  T(EQ(LENGTH(symbols_list), 2));
  T(EQ(INTERN("c"), INTERN("c")));
  T(EQ(LENGTH(symbols_list), 3));
  T(EQ(INTERN("d"), INTERN("d")));
  T(EQ(LENGTH(symbols_list), 4));

  that = INTERN("b");

  // Add a duplicate element so that we can see that both instances are removed:
  symbols_list = CONS(that, symbols_list);
  
  T(EQ(LENGTH(symbols_list), 5));
  T(MEMBERP(symbols_list, that));
  
  symbols_list = REMOVE(symbols_list, that);
  
  T(EQ(LENGTH(symbols_list), 3));
  T(NOT_MEMBERP(symbols_list, that));
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

  // deliberately avoid initializing symbols_list because INTERN2 should do so itself automatically.

  T(! strcmp(SYM_VAL(INTERN("one")), "one"));
  T(EQ(INTERN("one"), INTERN("one")));
  T(EQ(LENGTH(symbols_list), 1));
  
  T(! strcmp(SYM_VAL(INTERN("two")), "two"));
  T(NEQ(INTERN("one"), INTERN("two")));
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
    T( NEQL( (first)  , (second) ));                                                               \
    T( NEQL( (second) , (first)  ));                                                               \
  }

#define ETP(first, second)                                                                         \
  T( EQL( (first)  , (second) ));                                                                  \
  T( EQL( (second) , (first)  ));

#define SELF_EQL(o)                                                                              \
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
  
  T(NOT_MEMBERP(ENV_SYMS(this), INTERN("foo")));

  ENV_ADD(this, INTERN("foo"), NEW_INT(12));

  T(EQ(LENGTH(ENV_SYMS(this)), 1));
  T(EQ(LENGTH(ENV_VALS(this)), 1));
  T(MEMBERP(ENV_SYMS(this), INTERN("foo")));
  T(NOT_NILP(ENV_SYMS(this)));
  T(NOT_NILP(ENV_VALS(this)));

  T(NOT_MEMBERP(ENV_SYMS(this), INTERN("bar")));
  
  ENV_ADD(this, INTERN("bar"), NEW_INT(24));

  T(EQ(LENGTH(ENV_SYMS(this)), 2));
  T(EQ(LENGTH(ENV_VALS(this)), 2));
  T(MEMBERP(ENV_SYMS(this), INTERN("bar")));
  
  T(NOT_MEMBERP(ENV_SYMS(this), INTERN("baz")));

  ENV_ADD(this, INTERN("baz"), NEW_INT(36));

  T(EQ(LENGTH(ENV_SYMS(this)), 3));
  T(EQ(LENGTH(ENV_VALS(this)), 3));
  T(MEMBERP(ENV_SYMS(this), INTERN("baz")));
  T(EQ(INT_VAL(ENV_FIND(this, INTERN("foo"))), 12));
  T(EQ(INT_VAL(ENV_FIND(this, INTERN("bar"))), 24));
  T(EQ(INT_VAL(ENV_FIND(this, INTERN("baz"))), 36));

  that = NEW_ENV(NIL, NIL, NIL); // not yet linked to.

  ENV_ADD(that, INTERN("quux"), NEW_INT(48));

  T(NILP(ENV_FIND(this, INTERN("quux"))));

  ENV_PARENT(this) = that; // link this to that.
  
  T(EQ(INT_VAL(ENV_FIND(this, INTERN("quux"))), 48));
  T(EQ(ENV_FIND(this, INTERN("quux")), ENV_FIND(that, INTERN("quux"))));
  T(NILP(ENV_FIND(this, INTERN("zot"))));
  T(NILP(ENV_FIND(that, INTERN("foo"))));

  ENV_SET(this, INTERN("bar"), NEW_INT(99));
  
  T(EQ(INT_VAL(ENV_FIND(this, INTERN("bar"))), 99));

  ENV_SET(this, INTERN("zot"), NEW_INT(66));
  
  T(EQ(INT_VAL(ENV_FIND(this, INTERN("zot"))), 66));

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
  T(EQ(LENGTH(this)       , 3));
  TM("Expected length 3, got %d.", LENGTH(this));
  
  COUNT_LIST_LENGTH(this);
  T(EQ(list_length_counter, 3));
  T(EQ(list_length_counter, LENGTH(this)));

  T(shitty_princ_based_equality_predicate(this, "(1 2 3 . 4)"));

  ae_obj_t * mapped = MAP(this, ae_obj_double);
  T(shitty_princ_based_equality_predicate(mapped, "nil"));
  T(NILP(mapped));

  // PUT(NEW_CONS(NEW_INT(1), NEW_INT(2)));
}

ae_obj_t * make_args_containing_one_list(void) {
  return LIST(CONS(INTERN("a"), CONS(INTERN("b"), LIST(INTERN("c")))));
}

ae_obj_t * make_args_for_cons(void) {
  return CONS(NIL, LIST(CONS(INTERN("a"), CONS(INTERN("b"), LIST(INTERN("c"))))));
}

void core_cons_car_cdr(void) {
  SETUP_TEST;
  
  T(EQ(ae_core_car(make_args_containing_one_list()), INTERN("a")                                        ));
  T(EQ(ae_core_car(LIST(ae_core_cdr(make_args_containing_one_list()))), INTERN("b")                     ));
  T(shitty_princ_based_equality_predicate(ae_core_cons(CONS(INTERN("a"), LIST(NIL))), "(a)"             )); // cons 'a onto nil and get (a).
  T(shitty_princ_based_equality_predicate(ae_core_cdr (make_args_containing_one_list() ), "(b c)"       ));
  T(shitty_princ_based_equality_predicate(ae_core_cons(make_args_for_cons()            ), "(nil a b c)" ));
  T(NILP(ae_core_car(                 LIST(NIL))                                                        ));
  T(NILP(ae_core_cdr(                 LIST(NIL))                                                        ));
  T(NILP(ae_core_car(LIST(ae_core_car(LIST(NIL))))                                                      ));
  T(NILP(ae_core_cdr(LIST(ae_core_cdr(LIST(NIL))))                                                      ));
  T(NILP(ae_core_car(LIST(ae_core_cdr(LIST(NIL))))                                                      ));
  T(NILP(ae_core_cdr(LIST(ae_core_car(LIST(NIL))))                                                      ));
}

void core_eq_eql_atomp_not(void) {
  SETUP_TEST;
  
  this = CONS(NEW_INT(1), LIST(NEW_INT(2)));
  that = CONS(NEW_INT(1), LIST(NEW_INT(2)));

  T(TRUEP(ae_core_eql  (CONS(NEW_INT(5), LIST(NEW_INT  (5  ))                           )))); // 5 and 5 are equal numbers...
  T(NILP (ae_core_eq   (CONS(NEW_INT(5), LIST(NEW_INT  (5  ))                           )))); // ... but they are not the same object.
  T(TRUEP(ae_core_eql  (CONS(NEW_INT(5), LIST(NEW_FLOAT(5.0))                           )))); // 5 and 5.0 are equal-enough numbers...
  T(NILP (ae_core_eql  (CONS(NEW_INT(5), LIST(NEW_INT  (6  ))                           )))); // ... but 5 and 6 are not.
  
  T(TRUEP(ae_core_eq   (CONS(this      , LIST(this)                                     )))); // These are the same object and so are eq
  T(TRUEP(ae_core_eql  (CONS(this      , LIST(this)                                     )))); // and also eql.
  T(NILP (ae_core_eq   (CONS(this      , LIST(that)                                     )))); // These are the NOT the same object and so
  T(NILP (ae_core_eql  (CONS(this      , LIST(that)                                     )))); // neither eq or eql.
  T(NILP (ae_core_eq   (CONS(that      , LIST(this)                                     )))); // eq is commutative.
  T(NILP (ae_core_eql  (CONS(that      , LIST(this)                                     )))); // eql too.

  T(TRUEP(ae_core_eq   (CONS(this      , CONS(this         , LIST(this                 )))))); // eq can take 3+ arguments...
  T(NILP (ae_core_eq   (CONS(this      , CONS(this         , LIST(that                 ))))));
  T(TRUEP(ae_core_eq   (CONS(NIL       , CONS(NIL          , LIST(NIL                  ))))));
  T(NILP (ae_core_eq   (CONS(NIL       , CONS(NIL          , LIST(TRUE                 ))))));
    
  T(TRUEP(ae_core_eql  (CONS(NEW_INT(5), CONS(NEW_INT(5)   , LIST(NEW_INT(5)           )))))); // ...so can eql.
  T(NILP (ae_core_eql  (CONS(NEW_INT(5), CONS(NEW_INT(5)   , LIST(NEW_INT(6)           ))))));

  T(TRUEP(ae_core_atomp(CONS(NEW_INT(5), CONS(NEW_CHAR('a'), LIST(INTERN("a")          )))))); // These are all atoms.
  T(NILP (ae_core_atomp(CONS(NEW_INT(5), CONS(NEW_CHAR('a'), LIST(LIST(INTERN("a")))))))); // but these are not.

  T(TRUEP(ae_core_not  (CONS(NIL       , CONS(NIL          , LIST(NIL                  ))))));
  T(NILP (ae_core_not  (CONS(NIL       , CONS(NIL          , LIST(TRUE                 ))))));
}

void core_print_princ_write(void) {
  SETUP_TEST;

  NL;
  
  {
    PR("\nPrinting '\"hello\" 5 a abc' oo the next line: ");
    NL;
   
    ae_obj_t * written  = ae_core_write(CONS(NEW_STRING("hello"), CONS(NEW_INT(5), CONS(NEW_CHAR('a'), LIST(INTERN("abc"))))));
    NL;
    T(INT_VAL(written) == 17);
    TM("Expected %d, wrote %d.", 17, INT_VAL(written));
  }
  {
    PR("\nPrinting 'hello 5 a abc' oo the next line: ");
    ae_obj_t * written = ae_core_print(CONS(NEW_STRING("hello"), CONS(NEW_INT(5), CONS(NEW_CHAR('a'), LIST(INTERN("abc"))))));
    NL;
    T(INT_VAL(written) == 14);
    TM("Expected %d, wrote %d.", 14, INT_VAL(written));
  }
  {
    PR("\nPrinting 'hello5aabc' on the next line: ");
    NL;
    ae_obj_t * written = ae_core_princ(CONS(NEW_STRING("hello"), CONS(NEW_INT(5), CONS(NEW_CHAR('a'), LIST(INTERN("abc"))))));;
    NL;
    T(INT_VAL(written) == 10);
    TM("Expected %d, wrote %d.", 10, INT_VAL(written));
  }
  {
    PR("\nPrinting 'hello5aabc' on the next line: ");
    NL;
    ae_obj_t * written = ae_core_princ(CONS(NEW_STRING("hello"), CONS(NEW_INT(5), CONS(NEW_CHAR('a'), LIST(INTERN("abc"))))));
    NL;
    T(INT_VAL(written) == 10);
    TM("Expected %d, wrote %d.", 10, INT_VAL(written));
  }
}

void core_math(void) {
  SETUP_TEST;

  this = ae_core_add(CONS(NEW_INT(24), CONS(NEW_INT(4), LIST(NEW_INT(3) ))));
  T(EQL(this, NEW_INT(31)));
  
  this = ae_core_sub(CONS(NEW_INT(24), CONS(NEW_INT(4), LIST(NEW_INT(3) ))));
  T(EQL(this, NEW_INT(17)));

  this = ae_core_sub(CONS(NEW_INT(3),  CONS(NEW_INT(4), LIST(NEW_INT(24)))));
  T(EQL(this, NEW_INT(-25)));

  this = ae_core_mul(CONS(NEW_INT(24), CONS(NEW_INT(4), LIST(NEW_INT(3) ))));
  T(EQL(this, NEW_INT(288)));

  this = ae_core_div(CONS(NEW_INT(24), CONS(NEW_INT(4), LIST(NEW_INT(3) ))));
  T(EQL(this, NEW_INT(2)));
}

void core_cmp(void) {
  SETUP_TEST;
  
  T(TRUEP(ae_core_equal (CONS(NEW_INT(2), CONS(NEW_INT(2), LIST(NEW_INT(2)))))));
  T(NILP (ae_core_equal (CONS(NEW_INT(2), CONS(NEW_INT(2), LIST(NEW_INT(3)))))));

  T(NILP (ae_core_nequal(CONS(NEW_INT(2), CONS(NEW_INT(2), LIST(NEW_INT(2)))))));
  T(TRUEP(ae_core_nequal(CONS(NEW_INT(2), CONS(NEW_INT(2), LIST(NEW_INT(3)))))));

  T(TRUEP(ae_core_lt (CONS(NEW_INT(2), CONS(NEW_INT(4), LIST(NEW_INT(6)))))));
  T(NILP (ae_core_gt (CONS(NEW_INT(2), CONS(NEW_INT(4), LIST(NEW_INT(6)))))));

  T(TRUEP(ae_core_gt (CONS(NEW_INT(6), CONS(NEW_INT(4), LIST(NEW_INT(2)))))));
  T(NILP (ae_core_lt (CONS(NEW_INT(6), CONS(NEW_INT(4), LIST(NEW_INT(2)))))));

  T(TRUEP(ae_core_lte(CONS(NEW_INT(2), CONS(NEW_INT(4), CONS(NEW_INT(4), LIST(NEW_INT(6))))))));
  T(NILP (ae_core_gte(CONS(NEW_INT(2), CONS(NEW_INT(4), CONS(NEW_INT(4), LIST(NEW_INT(6))))))));
                                                                        
  T(TRUEP(ae_core_gte(CONS(NEW_INT(6), CONS(NEW_INT(4), CONS(NEW_INT(4), LIST(NEW_INT(2))))))));
  T(NILP (ae_core_lte(CONS(NEW_INT(6), CONS(NEW_INT(4), CONS(NEW_INT(4), LIST(NEW_INT(2))))))));
}

void root_env_and_eval(void) {
  SETUP_TEST;

  ae_obj_t * env    = ENV_NEW_ROOT();
  ae_obj_t * expr   = NIL;
  ae_obj_t * result = NIL;

  SETQ(env, INTERN("foo"), NEW_INT(666));

  T(EQL(NEW_INT(25),  EVAL(env, CONS(INTERN("+"), CONS(NEW_INT(16), LIST(NEW_INT(9)))))));
  T(EQL(NEW_INT(672), EVAL(env, CONS(INTERN("+"), CONS(NEW_INT(6), LIST(INTERN("foo")))))));

  T(EQL(NEW_INT(75),  EVAL(env, CONS(INTERN("*"), CONS(NEW_INT(3),  LIST(CONS(INTERN("+"), CONS(NEW_INT(16), LIST(NEW_INT(9))))))))));

  EVAL(env, CONS(INTERN("setq"), CONS(INTERN("bar"), LIST(NEW_INT(9)))));
  EVAL(env, CONS(INTERN("setq"), CONS(INTERN("baz"), LIST(CONS(INTERN("+"), CONS(NEW_INT(16), LIST(NEW_INT(9))))))));

  T(EQL(NEW_INT(9),   EVAL(env, INTERN("bar"))));
  T(EQL(NEW_INT(25),  EVAL(env, INTERN("baz"))));

  expr = CONS(INTERN("progn"), CONS(CONS(INTERN("princ"), LIST(NEW_STRING("Hello "))), CONS(CONS(INTERN("princ"), LIST(NEW_STRING("from Ash"))), LIST(CONS(INTERN("princ"), LIST(NEW_STRING("Lisp!")))))));

  NL;
  PR("Printing \"Hello from Ash Lisp!\" on the next line:\n");
  this = EVAL(env, expr);
  T(EQL(NEW_INT(5), this));
  NL;
  
  expr = CONS(INTERN("quote"), LIST(CONS(NEW_INT(5), CONS(NEW_INT(10), LIST(NEW_INT(15))))));
  T(shitty_princ_based_equality_predicate(EVAL(env, expr), "(5 10 15)"));

  expr = CONS(INTERN("quote"), LIST(INTERN("a")));
  T(shitty_princ_based_equality_predicate(EVAL(env, expr), "a"));

  expr = CONS(INTERN("if"), CONS(INTERN("t"), CONS(NEW_INT(11), CONS(INTERN("ignored"), LIST(NEW_INT(22))))));
  T(EQL(NEW_INT(11), EVAL(env, expr)));
  
  expr = CONS(INTERN("if"), CONS(INTERN("t"), LIST(NEW_INT(11))));
  T(EQL(NEW_INT(11), EVAL(env, expr)));

  expr = CONS(INTERN("if"), CONS(INTERN("nil"), CONS(NEW_INT(11), CONS(INTERN("ignored"), LIST(NEW_INT(22))))));
  T(EQL(NEW_INT(22), EVAL(env, expr)));
  
  expr = CONS(INTERN("if"), CONS(INTERN("nil"), LIST(NEW_INT(11))));
  T(NILP(EVAL(env, expr)));

  expr = CONS(INTERN("lambda"),
              CONS(LIST(INTERN("x")),
                   CONS(
                     CONS(INTERN("princ"), LIST(INTERN("x"))), LIST(
                       CONS(INTERN("+"),
                            CONS(INTERN("x"), LIST(NEW_INT(2))))))));

  result = EVAL(env, expr);

  T(LAMBDAP(result));
  
  ae_obj_t * subexpr = CONS(INTERN("*"), CONS(NEW_INT(3), LIST(NEW_INT(5))));
  
  expr = CONS(CONS(INTERN("lambda"),
                   CONS(LIST(INTERN("x")),
                        CONS(CONS(INTERN("princ"),
                                  LIST(INTERN("x"))),
                             LIST(CONS(INTERN("+"),
                                       CONS(INTERN("x"),
                                            LIST(subexpr))))))),
              LIST(NEW_INT(12)));


  PR("Printing 12 on the next line:\n");
  result = EVAL(env, expr);
  NL;

  // no princ: 
  expr = CONS(CONS(INTERN("lambda"),
                 CONS(LIST(INTERN("x")),
                      LIST(CONS(INTERN("+"),
                                CONS(INTERN("x"),
                                     LIST(NEW_INT(1))))))),
            LIST(CONS(INTERN("+"),
                      CONS(NEW_INT(2),
                           LIST(NEW_INT(3))))));

  PR("Printing 6 on the next line:\n");
  result = EVAL(env, expr);
  WRITE(result);
  NL;
  
  T(EQL(NEW_INT(6), result));

  ae_obj_t *    expr1 = CONS(CONS(INTERN("=="), CONS(INTERN("a"), LIST(NEW_INT(1)))), LIST(NEW_INT(10)));
  ae_obj_t *    expr2 = CONS(CONS(INTERN("=="), CONS(INTERN("a"), LIST(NEW_INT(2)))), LIST(NEW_INT(20)));
  ae_obj_t *    expr3 = CONS(TRUE, LIST(NEW_INT(30)));
  expr                = CONS(INTERN("cond"), CONS(expr1, CONS(expr2, LIST(expr3))));

  PR("Evaluating this cond: ");
  WRITE(expr);
  PR(".");
  NL;
  
#define TEST_COND(input, expected)                                                                            \
  {                                                                                                           \
    SETQ(env, INTERN("a"), NEW_INT(input));                                                                   \
    this = EVAL(env, expr);                                                                                   \
    PR("Result for " #input " is ");                                                                          \
    PRINC(this);                                                                                              \
    PR(", as expected.");                                                                                     \
    NL;                                                                                                       \
    if (NEQL(this, NEW_INT(expected))) {                                                                      \
      NL;                                                                                                     \
      PR("<this ");                                                                                           \
      WRITE(this);                                                                                            \
      PR(" == expected ");                                                                                    \
      WRITE(NEW_INT(expected));                                                                               \
      PR(">");                                                                                                \
      NL;                                                                                                     \
    }                                                                                                         \
    T(EQL(this, NEW_INT(expected)));                                                                          \
  }

  TEST_COND(1, 10);
  TEST_COND(2, 20);
  TEST_COND(3, 30);
}

void macros(void) {
  SETUP_TEST;

  NL;  

  ae_obj_t * and = ae_generate_macro_and();
  NL;
  PR("Got      "); PRINC(and); NL;
  PR("Wanted   (defmacro and args (cond ((null args) t) ((null (cdr args)) (car args)) (t (list (quote if) (car args) (cons (quote and) (cdr args))))))");
  NL;

  ae_obj_t * or = ae_generate_macro_or();
  NL;
  PR("Got      "); PRINC(or); NL;
  PR("Wanted   (defmacro or args (if (null args) nil (cons (quote cond) (mapcar list args))))");
  NL;

  ae_obj_t * defun = ae_generate_macro_defun();
  NL;
  PR("Got      "); PRINC(defun); NL;
  PR("Wanted   (defmacro defun (name params . body) (list (quote setq) name (list (quote lambda) params . body)))");
  NL;

  ae_obj_t * defmacro = ae_generate_macro_defmacro();
  NL;
  PR("Got      "); PRINC(defmacro); NL;
  PR("Wanted   (setq defmacro (macro (name params . body) (list (quote setq) name (list (quote macro) params . body))))");
  NL;

  NL;

  LOG(CONS(INTERN("test"), NIL) , "name");
  LOG(CONS(INTERN("x"), CONS(INTERN("y"), NIL)) , "args");
  LOG(CONS(INTERN("princ"), CONS(INTERN("x"), NIL)) , "args");

  NL;
  NL;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// TEST_LIST
////////////////////////////////////////////////////////////////////////////////////////////////////

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
  DO(intern_symbols)                                                                               \
  DO(remove_interned_symbol_from_list)                                                             \
  DO(truth)                                                                                        \
  DO(eql)                                                                                          \
  DO(fprinc_fwrite_lengths)                                                                        \
  DO(envs)                                                                                         \
  DO(core_cons_car_cdr)                                                                            \
  DO(core_eq_eql_atomp_not)                                                                        \
  DO(core_print_princ_write)                                                                       \
  DO(core_math)                                                                                    \
  DO(core_cmp)                                                                                     \
  DO(root_env_and_eval)                                                                            \
  DO(improper_list)                                                                                \
  DO(macros)

#define pair(fun) { #fun, fun },

TEST_LIST = {
  FOR_EACH_TEST_FUN(pair)
  { NULL, NULL }
};
