#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>

#include "ae_obj.h"

#include "acutest.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// Macros
////////////////////////////////////////////////////////////////////////////////////////////////////

#define T  TEST_CHECK
#define TM TEST_MSG

#define NL putchar('\n');
#define FF fflush(stdout);

#define COUNT_LIST_LENGTH(l) list_length_counter = 0; EACH((l), incr_list_length_counter);

#define SETUP_TEST                                                                                 \
  static char * tmp_str = NULL;                                                                    \
  ae_obj_t *    this    = NULL;                                                                    \
  ae_obj_t *    that    = NULL;                                                                    \
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

char * write_to_new_string(const ae_obj_t * const this) {
  char * buff;
  size_t size;
  FILE * stream = open_memstream(&buff, &size);

  ae_obj_fwrite(this, stream);
  fclose(stream);

  return buff;
}

bool shitty_write_based_equality_predicate(
  const ae_obj_t * const this,
  const char * const strcmp_str
) {
  return ! strcmp(strcmp_str, SWRITE(this));
}

ae_obj_t * push_together_a_list_of_ints(void) {
  ae_obj_t * list    = NEW(AE_CONS);
  ae_obj_t * tailtip = list;

  T(LENGTH(tailtip) == 0);

  for (unsigned int ix = 0; ix < 4; ix++) { 
    ae_obj_t * new_int = NEW(AE_INTEGER);
    int        int_val = ix + 1;
    new_int->int_val   = int_val;

    tailtip              = PUSH(tailtip, new_int);
    
    T(CONSP(tailtip));
    T(INTEGERP(CAR(tailtip)));
    T(INT_VAL(CAR(tailtip)) == int_val);
    T(LENGTH(list) == ix + 1);
    TM("Length is %zu.", LENGTH(tailtip));
  }

  return list;
}

ae_obj_t * cons_together_a_list_of_ints(void) {
  ae_obj_t * list = NEW(AE_CONS);
  ae_obj_t * head = NEW(AE_INTEGER);
  head->int_val = 4;
  CAR(list)       = head;
  
  T(LENGTH(list) == 1);

  for (unsigned int ix = 0; ix < 3; ix++) { 
    ae_obj_t * new_head = NEW(AE_INTEGER);
    new_head->int_val = 3 - ix;

    ae_obj_t * tail = list;
    
    list = CONS(new_head, tail);

    const size_t expected_length = 2 + ix;
  
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

static size_t list_length_counter = 0;

void incr_list_length_counter(ae_obj_t * const this) {
  (void)this;
  list_length_counter++;
}

ae_obj_t * ae_obj_double(ae_obj_t * const this) {
  ASSERT_INTEGERP(this);

  ae_obj_t * new_obj = NEW(AE_INTEGER);
  new_obj->int_val = this->int_val * 2;

  return new_obj;
}

ae_obj_t * ae_obj_to_pairs(ae_obj_t * const this) {
  ae_obj_t * new_list = NEW(AE_CONS);
  
  CAR(new_list) = (ae_obj_t *)this;

  return CONS((ae_obj_t *)this, new_list);
}

void basic_list_checks(ae_obj_t * this) {
  COUNT_LIST_LENGTH(this);

  T(LENGTH(this) == 4);
  T(list_length_counter == 4);
  T(list_length_counter == LENGTH(this));
  T(shitty_write_based_equality_predicate(this, "(1 2 3 4 \b) "));
  T(shitty_write_based_equality_predicate(MAP(this, ae_obj_double), "(2 4 6 8 \b) "));
  T(shitty_write_based_equality_predicate(CLONE(MAP(this, ae_obj_double)), "(2 4 6 8 \b) "));
  T(shitty_write_based_equality_predicate(CLONE(MAP(this, ae_obj_to_pairs)), "((1 1 \b) (2 2 \b) (3 3 \b) (4 4 \b) \b) "));
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Tests
////////////////////////////////////////////////////////////////////////////////////////////////////

void remove_elem_from_list(void) {
  SETUP_TEST;

  // re-use 'this' as the symbol list here:
  T(EQ(INTERN(&this, "a"), INTERN(&this, "a")));
  T(EQ(INTERN(&this, "b"), INTERN(&this, "b")));
  T(EQ(INTERN(&this, "c"), INTERN(&this, "c")));
  T(EQ(INTERN(&this, "d"), INTERN(&this, "d")));
  T(LENGTH(this) == 4);

  that = INTERN(&this, "b");

  T(MEMBER(this, that));
  
  this = REMOVE(this, that);
  
  T(! MEMBER(this, that));
  T(shitty_write_based_equality_predicate(this, "(d c a \b) "));
  T(LENGTH(this) == 3);

  /* TODO: Add a case testing the removal of multiple instances of the same item. */
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
                                                                                                   \
    this = NEW(_type);                                                                             \
                                                                                                   \
    T(TYPE(this) == _type);                                                                        \
  }
  FOR_EACH_LEXED_TYPE(test);
}

void newly_initialized_ae_obj_has_zeroed_data_fields(void) {
  SETUP_TEST;

  this = NEW(AE_RATIONAL);

  T(this->numerator_val == 0 && this->denominator_val == 0);
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

void pushed_and_consed_lists_write_identically(void) {
  SETUP_TEST;

  this    = push_together_a_list_of_ints();
  tmp_str = SWRITE(this);

  T(shitty_write_based_equality_predicate(cons_together_a_list_of_ints(), tmp_str));
}

void intern_symbols(void) {
  SETUP_TEST;

  // re-use 'this' as the symbol list here:
  T(INTERN(&this, "one") == INTERN(&this, "one"));
  T(INTERN(&this, "one") != INTERN(&this, "two"));
  T(LENGTH(this) == 2);
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
    T((int)strlen(buff) == (int)size);                                                             \
    TM("strlen was %d but size was %d:\n\"%s\".\n",                                                \
       (int)strlen(buff), (int)size, buff);                                                        \
    T((int)strlen(buff) == (int)reported);                                                         \
    TM("strlen was %d but reported was %d:\n\"%s\".\n",                                            \
       (int)strlen(buff), (int)reported, buff);                                                    \
  }

void fwrite_lengths(void) {
  SETUP_TEST;

  FWRITE_TEST(AE_CHAR, int_val,       '1'                                 );
  FWRITE_TEST(AE_CHAR, int_val,       '\n'                                );
  FWRITE_TEST(AE_INTEGER, int_val,       123                                 );
  FWRITE_TEST(AE_FLOAT, float_val,     1.23                                );
  FWRITE_TEST(AE_RATIONAL, numerator_val, 123,   this->denominator_val = 456; );
  FWRITE_TEST(AE_STRING, str_val,       "asdf"                              );
  FWRITE_TEST(AE_SYMBOL, sym_val,       "ghij"                              );
}

void truth(void) {
  SETUP_TEST;

  this = ae_obj_truth(true);

  T(INTEGERP(this) && INT_VAL(this) == 1);
  
  that = ae_obj_truth(false);

  T(CONSP(that) && (! CAR(that)) && (! CDR(that)));
}

void equal(void) {
  SETUP_TEST;

  ae_obj_t * obj_int_2a      = NEW(AE_INTEGER);
  INT_VAL   (obj_int_2a)     = 2;

  ae_obj_t * obj_int_2b      = NEW(AE_INTEGER);
  INT_VAL   (obj_int_2b)     = 2;

  ae_obj_t * obj_float_2a    = NEW(AE_FLOAT);
  FLOAT_VAL (obj_float_2a)   = 2.0;

  ae_obj_t * obj_float_2b    = NEW(AE_FLOAT);
  FLOAT_VAL (obj_float_2b)   = 2.0;

  ae_obj_t * obj_int_3a      = NEW(AE_INTEGER);  // not yet used
  INT_VAL   (obj_int_3a)     = 3;

  ae_obj_t * obj_int_3b      = NEW(AE_INTEGER);  // not yet used
  INT_VAL   (obj_int_3b)     = 3;

  ae_obj_t * obj_float_3a    = NEW(AE_FLOAT);    // not yet used
  FLOAT_VAL (obj_float_3a)   = 3.0;

  ae_obj_t * obj_float_3b    = NEW(AE_FLOAT);    // not yet used
  FLOAT_VAL (obj_float_3b)   = 3.0;

  ae_obj_t * obj_bool_false  = ae_obj_truth(false);
  ae_obj_t * obj_bool_true   = ae_obj_truth(obj_float_2a);

  ae_obj_t * obj_list_consed = cons_together_a_list_of_ints();
  ae_obj_t * obj_list_pushed = push_together_a_list_of_ints();

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
    X(obj_list_pushed)

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
  
  //  Some numbers are equal to each other.
  ETP( obj_float_2a , obj_int_2b   );
  ETP( obj_int_2a   , obj_int_2b   );
  ETP( obj_int_2b   , obj_float_2a );
  ETP( obj_int_2b   , obj_int_2a   );

  ETP( obj_float_3a , obj_int_3b   );
  ETP( obj_int_3a   , obj_int_3b   );
  ETP( obj_int_3b   , obj_float_3a );
  ETP( obj_int_3b   , obj_int_3a   );

  //  Some numbers are not equal to each other.
  NETP( obj_int_2a   , obj_int_3a   );
  NETP( obj_int_2a   , obj_int_3a   );
  NETP( obj_int_2b   , obj_float_3a );
  NETP( obj_int_2b   , obj_int_3a   );

  NETP( obj_float_2a , obj_int_3a   );
  NETP( obj_float_2a , obj_int_3a   );
  NETP( obj_float_2b , obj_float_3a );
  NETP( obj_float_2b , obj_int_3a   );

  // These aren't equal to anything other than themselves:
#define XX(other) NETP(obj_bool_false, other);
  FOR_EVERY_OBJ_DO(XX)
#undef XX

#define XX(other) NETP(obj_bool_true, other);
  FOR_EVERY_OBJ_DO(XX)
#undef XX

#define XX(other) NETP(obj_list_consed, other);
  FOR_EVERY_OBJ_DO(XX)
#undef XX
                                                         
#define XX(other) NETP(obj_list_pushed, other);
  FOR_EVERY_OBJ_DO(XX)
#undef XX

    /* todo: add tests for rationals */
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
  DO(pushed_list_tests)                                                                            \
  DO(consed_list_tests)                                                                            \
  DO(pushed_and_consed_lists_write_identically)                                                    \
  DO(intern_symbols)                                                                               \
  DO(remove_elem_from_list)                                                                        \
  DO(truth)                                                                                        \
  DO(equal)                                                                                        \
  DO(fwrite_lengths)

/* TODO: write ae_obj_remove_elem_from and a test for it. */

#define pair(fun) { #fun, fun },

TEST_LIST = {
  FOR_EACH_TEST_FUN(pair)
  { NULL, NULL }
};
