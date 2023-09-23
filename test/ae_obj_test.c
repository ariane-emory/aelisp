
#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BEFORE_ACUTEST

#include "ae_obj.h"
#include "ae_free_list.h"
#include "ae_lisp_primitives.h"

#include "acutest.h"

#define free_list_size (1 << 12)

static char mem[free_list_size] = { 0 };

////////////////////////////////////////////////////////////////////////////////////////////////////
// Macros
////////////////////////////////////////////////////////////////////////////////////////////////////

#define T       TEST_CHECK
#define TM      TEST_MSG
#define NL      putchar('\n');
#define FF      fflush(stdout);
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

char * write_to_new_string(const ae_obj_t * const this) {
  char * buff;
  size_t size;
  FILE * stream = open_memstream(&buff, &size);

  FWRITE(this, stream);
  fclose(stream);

  return buff;
}

bool shitty_write_based_equality_predicate(
  const ae_obj_t * const this,
  const char * const strcmp_str) {
  return ! strcmp(strcmp_str, SWRITE(this));
}

ae_obj_t * push_together_a_list_of_ints(void) {
  ae_obj_t * num     = NEW_INT(1);
  ae_obj_t * list    = CONS_NIL(num);
  ae_obj_t * tailtip = list;

  T(EQ(LENGTH(tailtip), 1));

  for (int ix = 2; ix < 5; ix++) { 
    int        int_val = ix;
    ae_obj_t * new_int = NEW_INT(int_val);

    tailtip            = PUSH(&tailtip, new_int);
    
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
  ae_obj_t * list = CONS_NIL(head);
  
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
  return CONS(this, CONS_NIL(this));
}

void basic_list_checks(ae_obj_t * this) {
  COUNT_LIST_LENGTH(this);

  T(EQ(LENGTH(this)       , 4));
  T(EQ(list_length_counter, 4));
  T(EQ(list_length_counter, LENGTH(this)));

  // WRITE(this); NL;
  
  T(shitty_write_based_equality_predicate(this, "(1 2 3 4)"));
  tmp_str = SWRITE(this); TM("Got \"%s\".", tmp_str);

  ae_obj_t * mapped = NULL;

  mapped = MAP(this, ae_obj_double);
  // fprintf(stdout, "doubled "); WRITE(mapped); NL;
  // pool_print();
  T(shitty_write_based_equality_predicate(mapped, "(2 4 6 8)"));
  tmp_str = SWRITE(this); TM("Got \"%s\".", tmp_str);

  mapped = CLONE(mapped);
  // fprintf(stdout, "cloned  "); WRITE(mapped); NL;
  // pool_print();
  T(shitty_write_based_equality_predicate(mapped, "(2 4 6 8)"));
  tmp_str = SWRITE(this); TM("Got \"%s\".", tmp_str);

  mapped = MAP(mapped, ae_obj_to_pairs);
  // fprintf(stdout, "paired  ");   WRITE(mapped); NL;
  // pool_print();
  T(shitty_write_based_equality_predicate(mapped, "((2 2) (4 4) (6 6) (8 8))"));
  tmp_str = SWRITE(this); TM("Got \"%s\".", tmp_str);

  // pool_print();
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
  T(shitty_write_based_equality_predicate(symbols_list, "(d c a)"));
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

void pushed_and_consed_lists_write_identically(void) {
  SETUP_TEST;
  this    = push_together_a_list_of_ints();
  tmp_str = SWRITE(this);
  T(shitty_write_based_equality_predicate(cons_together_a_list_of_ints(), tmp_str));
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

#define FWRITE_TEST(type, field, val, ...)                                                         \
  {                                                                                                \
    this        = NEW(type);                                                                       \
    this->field = val;                                                                             \
                                                                                                   \
    __VA_ARGS__;                                                                                   \
                                                                                                   \
    char * buff;                                                                                   \
    size_t size;                                                                                   \
    FILE * stream   = open_memstream(&buff, &size);                                                \
    int    reported = ae_obj_fwrite(this, stream);                                                 \
                                                                                                   \
    fclose(stream);                                                                                \
    free(buff);                                                                                    \
                                                                                                   \
    T(EQ((int)strlen(buff), (int)size));                                                           \
    TM("strlen was %d but size was %d:\n\"%s\".\n",                                                \
       (int)strlen(buff), (int)size, buff);                                                        \
    T(EQ((int)strlen(buff), (int)reported));                                                       \
    TM("strlen was %d but reported was %d:\n\"%s\".\n",                                            \
       (int)strlen(buff), (int)reported, buff);                                                    \
  }

void fwrite_lengths(void) {
  SETUP_TEST;

  FWRITE_TEST(AE_CHAR,     char_val,      '1'                                 );
  FWRITE_TEST(AE_CHAR,     char_val,      '\n'                                );
  FWRITE_TEST(AE_INTEGER,  int_val,       123                                 );
  FWRITE_TEST(AE_FLOAT,    float_val,     1.23                                );
  FWRITE_TEST(AE_RATIONAL, numerator_val, 123,   this->denominator_val = 456; );
  FWRITE_TEST(AE_STRING,   str_val,       "asdf"                              );
  FWRITE_TEST(AE_SYMBOL,   sym_val,       "ghij"                              );
}

void equal(void) {
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

#define SELF_EQUAL(o)                                                                              \
  ETP( obj_bool_false , obj_bool_false );

  //  Everything is equal to itself.
  FOR_EVERY_OBJ_DO(SELF_EQUAL);

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

  this = NEW_ENV(NIL);

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

  that = NEW_ENV(NIL); // not yet linked to.

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
  WRITE(ENV_SYMS(that));
  NL;
  PR("  vals ");
  WRITE(ENV_VALS(that));
  NL;
  PR("Lower:\n");
  PR("  syms ");
  WRITE(ENV_SYMS(this));
  NL;
  PR("  vals ");
  WRITE(ENV_VALS(this));
  NL;
#endif
}

ae_obj_t * make_args_containing_one_list(void) {
  ae_obj_t * sym_a = INTERN("b");
  ae_obj_t * sym_b = INTERN("c");
  ae_obj_t * sym_c = INTERN("d");
  ae_obj_t * args  = CONS_NIL(CONS(sym_a, CONS(sym_b, CONS_NIL(sym_c))));

  return args;
}

void primitive_cons_car_cdr(void) {
  SETUP_TEST;

  ae_obj_t * args  = make_args_containing_one_list();
  
  NL;
  PR("Built ");
  WRITE(args);
  NL;

  ae_obj_t * car = ae_lisp_car(args);
  ae_obj_t * cdr = ae_lisp_cdr(args);

  PR("car = ");
  WRITE(car);
  NL;

  PR("cdr = ");
  WRITE(cdr);
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
  DO(pushed_and_consed_lists_write_identically)                                                    \
  DO(intern_symbols)                                                                               \
  DO(remove_interned_symbol_from_list)                                                             \
  DO(truth)                                                                                        \
  DO(equal)                                                                                        \
  DO(fwrite_lengths)                                                                               \
  DO(envs)                                                                                         \
  DO(primitive_cons_car_cdr)

#define pair(fun) { #fun, fun },

TEST_LIST = {
  FOR_EACH_TEST_FUN(pair)
  { NULL, NULL }
};
