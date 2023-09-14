#include <stdio.h>
#include <stdbool.h>

#include "ae_obj.h"
#include "acutest.h"

#define NL   putchar('\n')
#define FAIL TEST_CHECK(0)
#define T    TEST_CHECK

#define SETUP_TEST                                                                                                                          \
  pool_clear();                                                                                                                             \
  if (tmp_str) {                                                                                                                            \
    free(tmp_str);                                                                                                                          \
    tmp_str = 0;                                                                                                                            \
  }                                                                                                                                         \
  ae_obj_t * this = ALLOC_AE_OBJ;                                                                                                           \
  ae_obj_t * that = ALLOC_AE_OBJ;                                                                                                           \
  T(!!this);                                                                                                                                \
  T(!!that);                                                                                                                                \
  T(this != that);                                                                                                                          \
  list_counter = 0;                                                                                                                         \
  (void)that;

char * tmp_str = 0;

////////////////////////////////////////////////////////////////////////////////
// Helpers
////////////////////////////////////////////////////////////////////////////////

char * write_to_new_string(const ae_obj_t * const this) {
  char * buff;
  size_t size;
  FILE * stream = open_memstream(&buff, &size);

  ae_obj_fwrite(this, stream);
  fclose(stream);

  return buff;
}

bool shitty_write_based_equality_predicate(const ae_obj_t * const this, const char * const strcmp_str) {
  return ! strcmp(strcmp_str, tmp_str = write_to_new_string(this));
}

ae_obj_t * push_together_a_list_of_ints(void) {
  ae_obj_t *  new_list = ALLOC_AE_OBJ;
  ae_obj_init(new_list,  AE_CONS____);

  T(ae_obj_length(new_list) == 0);

  for (unsigned int ix = 0; ix < 4; ix++) { 
    ae_obj_t * new_tailtip = ALLOC_AE_OBJ;
    ae_obj_init(new_tailtip, AE_INTEGER_);
    new_tailtip->int_value = ix + 1;

    ae_obj_push_back(new_list, new_tailtip);

    T(ae_obj_length(new_list) == ix + 1);
  }

  return new_list;
}

ae_obj_t * cons_together_a_list_of_ints(void) {
  ae_obj_t * new_list   = ALLOC_AE_OBJ;
  ae_obj_t * head       = ALLOC_AE_OBJ;
  ae_obj_init(new_list,   AE_CONS____);
  ae_obj_init(head,       AE_INTEGER_);

  new_list->head = head;
  head->int_value = 4;
  
  T(ae_obj_length(new_list) == 1);

  for (unsigned int ix = 0; ix < 3; ix++) { 
    ae_obj_t * new_head = ALLOC_AE_OBJ;
    ae_obj_init(new_head, AE_INTEGER_);
    new_head->int_value = 3 - ix;

    ae_obj_t * tail = new_list;
    
    new_list = CONS(new_head, tail);

    const size_t expected_length = 2 + ix;
  
    T(new_list != head);
    T(new_list != new_head);
    T(new_list->head == new_head);
    T(new_list->tail == tail);
    T(ae_obj_length(new_list) == expected_length);
    TEST_MSG(
      "Incorrect length %d, expected %d.",
      ae_obj_length(new_list),
      expected_length);
  }
  
  return new_list;
}

static size_t list_counter = 0;

void incr_list_counter(ae_obj_t * const this) {
  (void)this;
  list_counter++;
}

ae_obj_t * ae_obj_double(ae_obj_t * const this) {
  ASSERT_INTEGERP(this);

  ae_obj_t * that = ALLOC_AE_OBJ;
  ae_obj_init(that, AE_INTEGER_);

  that->int_value = this->int_value * 2;

  return that;
}

ae_obj_t * ae_obj_to_pairs(ae_obj_t * const this) {
  ae_obj_t * new_list = ALLOC_AE_OBJ;
  ae_obj_init(new_list, AE_CONS____);
  
  // This cast might be a little sketch? Think about it...
  new_list->head = (ae_obj_t *)this;

  return CONS((ae_obj_t *)this, new_list);
}

void basic_list_checks(ae_obj_t * this) {
  ae_obj_each(this, incr_list_counter); 

  T(list_counter == 4);
  T(ae_obj_length(this) == 4);
  T(shitty_write_based_equality_predicate(this, "(1 2 3 4 \b) "));
  T(shitty_write_based_equality_predicate(ae_obj_map(this, ae_obj_double), "(2 4 6 8 \b) "));
  T(shitty_write_based_equality_predicate(ae_obj_clone(ae_obj_map(this, ae_obj_double)), "(2 4 6 8 \b) "));
  T(shitty_write_based_equality_predicate(ae_obj_clone(ae_obj_map(this, ae_obj_to_pairs)), "((1 1 \b) (2 2 \b) (3 3 \b) (4 4 \b) \b) "));}

////////////////////////////////////////////////////////////////////////////////
// Tests
////////////////////////////////////////////////////////////////////////////////

void newly_allocated_ae_obj_is_inside_pool(void)
{
  SETUP_TEST;

  T(this >= pool_first && this <= pool_last);
  TEST_MSG("obj @ %p is outside of pool (pool begins at %p, ends at %p).", this, pool_first, pool_last);
}

void newly_allocated_ae_obj_type_is_AE_INVALID_(void)
{
  SETUP_TEST;

  T(this->type == AE_INVALID_);
}

void newly_initialized_ae_obj_has_correct_type_field(void) {
#define test(_type)                                                                                                                         \
  {                                                                                                                                         \
    SETUP_TEST;                                                                                                                             \
                                                                                                                                            \
    ae_obj_init(this, _type);                                                                                                               \
                                                                                                                                            \
    T(this->type == _type);                                                                                                                 \
  }
  FOR_LEXED_TYPES_DO(test);
}

void newly_initialized_ae_obj_has_zeroed_data_fields(void) {
  SETUP_TEST;
  
  ae_obj_init(this, AE_RATIONAL);

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

  ae_obj_init(this, AE_CHAR____);
  this->char_value = 'x';

  ae_obj_init(that, AE_RATIONAL);
  that->numerator_value   = 123;
  that->denominator_value = 321;

  ae_obj_unsafe_move(this, that);

  T(this->type              == AE_RATIONAL);
  T(this->numerator_value   == 123);
  T(this->denominator_value == 321);

  T(that->type              == AE_FREE____);
  T(that->numerator_value   == 0);
  T(that->denominator_value == 0);
}

void clone_a_simple_ae_obj(void) {
  SETUP_TEST;

  ae_obj_init(this, AE_RATIONAL);
  this->numerator_value   = 123;
  this->denominator_value = 321;

  ae_obj_t * clone = ae_obj_clone(this);

  T(this != that);
  T(clone->type              == AE_RATIONAL);
  T(clone->numerator_value   == 123);
  T(clone->denominator_value == 321);
}

void pushed_and_consed_lists_write_identically(void) {
  SETUP_TEST;

  this    = push_together_a_list_of_ints();
  that    = cons_together_a_list_of_ints();
  tmp_str = write_to_new_string(that);

  T(shitty_write_based_equality_predicate(this, tmp_str));
}

#define FOR_TEST_FUNS_DO(X)                                                                                                                 \
  X(newly_allocated_ae_obj_is_inside_pool)                                                                                                  \
  X(newly_allocated_ae_obj_type_is_AE_INVALID_)                                                                                             \
  X(newly_initialized_ae_obj_has_correct_type_field)                                                                                        \
  X(newly_initialized_ae_obj_has_zeroed_data_fields)                                                                                        \
  X(unsafe_move_an_ae_obj)                                                                                                                  \
  X(clone_a_simple_ae_obju)                                                                                                                 \
  X(pushed_list_tests)                                                                                                                      \
  X(consed_list_tests)                                                                                                                      \
  X(pushed_and_consed_lists_write_identically)

/* TODO: write ae_obj_remove_elem_from and a test for it. */

#define pair(fun) { #fun, fun },

TEST_LIST = {
  FOR_TEST_FUNS_DO(pair)
  { NULL, NULL }
};
