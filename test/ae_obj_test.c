#include <stdio.h>
#include <stdbool.h>
#include <stddef.h>

#include "ae_obj.h"
#include "acutest.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// Macros
////////////////////////////////////////////////////////////////////////////////////////////////////

#define T TEST_CHECK

#define COUNT_LIST_LENGTH(l) list_length_counter = 0; EACH((l), incr_list_length_counter);

#define SETUP_TEST                                                                                                                          \
  static char * tmp_str = NULL;                                                                                                             \
  ae_obj_t *    this    = NULL;                                                                                                             \
  ae_obj_t *    that    = NULL;                                                                                                             \
  pool_clear();                                                                                                                             \
  if (tmp_str) {                                                                                                                            \
    free(tmp_str);                                                                                                                          \
    tmp_str = NULL;                                                                                                                         \
  }                                                                                                                                         \
  (void)this;                                                                                                                               \
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

bool shitty_write_based_equality_predicate(const ae_obj_t * const this, const char * const strcmp_str) {
  return ! strcmp(strcmp_str, write_to_new_string(this));
}

ae_obj_t * push_together_a_list_of_ints(void) {
  ae_obj_t *  new_list = NEW(AE_CONS____);

  T(LENGTH(new_list) == 0);

  for (unsigned int ix = 0; ix < 4; ix++) { 
    ae_obj_t * new_tailtip = NEW(AE_INTEGER_);
    new_tailtip->int_value = ix + 1;
    PUSH(new_list, new_tailtip);

    T(LENGTH(new_list) == ix + 1);
  }

  return new_list;
}

ae_obj_t * cons_together_a_list_of_ints(void) {
  ae_obj_t * list = NEW(AE_CONS____);
  ae_obj_t * head = NEW(AE_INTEGER_);
  head->int_value = 4;
  CAR(list)       = head;
  
  T(LENGTH(list) == 1);

  for (unsigned int ix = 0; ix < 3; ix++) { 
    ae_obj_t * new_head = NEW(AE_INTEGER_);
    new_head->int_value = 3 - ix;

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

  ae_obj_t * new_obj = NEW(AE_INTEGER_);
  new_obj->int_value = this->int_value * 2;

  return new_obj;
}

ae_obj_t * ae_obj_to_pairs(ae_obj_t * const this) {
  ae_obj_t * new_list = NEW(AE_CONS____);
  
  // This cast might be a little sketch? Think about it...
  new_list->head = (ae_obj_t *)this;

  return CONS((ae_obj_t *)this, new_list);
}

void basic_list_checks(ae_obj_t * this) {
  COUNT_LIST_LENGTH(this);

  T(list_length_counter == 4);
  T(LENGTH(this) == 4);
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
  T(INTERN(&this, "a") == INTERN(&this, "a"));
  T(INTERN(&this, "b") == INTERN(&this, "b"));
  T(INTERN(&this, "c") == INTERN(&this, "c"));
  T(INTERN(&this, "d") == INTERN(&this, "d"));
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

void newly_allocated_ae_obj_type_is_AE_INVALID_(void)
{
  SETUP_TEST;

  T(ALLOC()->type == AE_INVALID_);
}

void newly_initialized_ae_obj_has_correct_type_field(void) {
#define test(_type)                                                                                                                         \
  {                                                                                                                                         \
    SETUP_TEST;                                                                                                                             \
                                                                                                                                            \
    this = NEW(_type);                                                                                                                      \
                                                                                                                                            \
    T(this->type == _type);                                                                                                                 \
  }
  FOR_EACH_LEXED_TYPE(test);
}

void newly_initialized_ae_obj_has_zeroed_data_fields(void) {
  SETUP_TEST;

  this = NEW(AE_RATIONAL);

  T(this->numerator_value == 0 && this->denominator_value == 0);
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
  that->numerator_value   = 123;
  that->denominator_value = 321;

  this = MOVE_NEW(that);

  T(this->type              == AE_RATIONAL);
  T(this->numerator_value   == 123);
  T(this->denominator_value == 321);

  T(that->type              == AE_FREE____);
  T(that->numerator_value   == 0);
  T(that->denominator_value == 0);
}

void clone_a_simple_ae_obj(void) {
  SETUP_TEST;

  this = NEW(AE_RATIONAL);
  this->numerator_value   = 123;
  this->denominator_value = 321;

  that = CLONE(this);

  T(this != that);
  T(that->type              == AE_RATIONAL);
  T(that->numerator_value   == 123);
  T(that->denominator_value == 321);
}

void pushed_and_consed_lists_write_identically(void) {
  SETUP_TEST;

  this    = push_together_a_list_of_ints();
  tmp_str = write_to_new_string(this);

  T(shitty_write_based_equality_predicate(cons_together_a_list_of_ints(), tmp_str));
}

void intern_symbols(void) {
  SETUP_TEST;

  // re-use 'this' as the symbol list here:
  T(INTERN(&this, "one") == INTERN(&this, "one"));
  T(INTERN(&this, "one") != INTERN(&this, "two"));
  T(LENGTH(this) == 2);
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// TEST_LIST
////////////////////////////////////////////////////////////////////////////////////////////////////

#define FOR_EACH_TEST_FUN(DO)                                                                                                               \
  DO(test_setup_is_okay)                                                                                                                    \
  DO(newly_allocated_ae_obj_is_inside_pool)                                                                                                 \
  DO(newly_allocated_ae_obj_type_is_AE_INVALID_)                                                                                            \
  DO(newly_initialized_ae_obj_has_correct_type_field)                                                                                       \
  DO(newly_initialized_ae_obj_has_zeroed_data_fields)                                                                                       \
  DO(unsafe_move_an_ae_obj)                                                                                                                 \
  DO(clone_a_simple_ae_obj)                                                                                                                 \
  DO(pushed_list_tests)                                                                                                                     \
  DO(consed_list_tests)                                                                                                                     \
  DO(pushed_and_consed_lists_write_identically)                                                                                             \
  DO(intern_symbols)                                                                                                                        \
  DO(remove_elem_from_list)

/* TODO: write ae_obj_remove_elem_from and a test for it. */

#define pair(fun) { #fun, fun },

TEST_LIST = {
  FOR_EACH_TEST_FUN(pair)
  { NULL, NULL }
};
