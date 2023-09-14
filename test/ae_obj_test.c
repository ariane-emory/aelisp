#include <stdio.h>

#include "ae_obj.h"
#include "acutest.h"

#define NL putchar('\n')
#define FAIL TEST_CHECK(0)
#define T TEST_CHECK

#define SETUP_TEST                                                                                                                          \
  pool_clear();                                                                                                                             \
  ae_obj_t * this = ALLOC_AE_OBJ;                                                                                                           \
  ae_obj_t * that = ALLOC_AE_OBJ;                                                                                                           \
  T(this != that);                                                                                                                          \
  size_t counter = 1;                                                                                                                       \
  (void)counter;                                                                                                                            \
  (void)that;

void newly_allocated_ae_obj_is_inside_pool(void)
{
  SETUP_TEST;
  T(this >= pool_first && this <= pool_last);
  TEST_MSG("obj @ %p is outside of pool (pool begins at %p, ends at %p).", this, pool_first, pool_last);
}

void newly_initialized_ae_obj_has_correct_type_field(void) {
#define test(_type)                                                                                                                         \
  {                                                                                                                                         \
    SETUP_TEST;                                                                                                                             \
    ae_obj_init(this, _type);                                                                                                               \
    T(this->type == _type);                                                                                                                 \
    TEST_MSG("After ae_obj_init(obj, " #_type "), obj->type != " #_type ".");                                                               \
  }
  FOR_LEXED_TYPES_DO(test);
}

void newly_initialized_ae_obj_has_zeroed_data_fields(void) {
  SETUP_TEST;
  ae_obj_init(this, AE_RATIONAL);
  T(this->numerator_value == 0 && this->denominator_value == 0);
  TEST_MSG("After ae_obj_init(obj, %s), its data fields should == 0.", ae_type_str(this->type));
}

static size_t cons_and_each_tests_length = 0;

void incr_cons_and_each_tests_length(ae_obj_t * const this) {
  (void)this;
  cons_and_each_tests_length++;
}

ae_obj_t * ae_obj_double(const ae_obj_t * const this) {
  ASSERT_INTEGERP(this);

  ae_obj_t * that = ALLOC_AE_OBJ;
  ae_obj_init(that, AE_INTEGER_);
  that->int_value = this->int_value * 2;

  return that;
}

void cons_and_each(void) {
  SETUP_TEST;

  ae_obj_init(this, AE_CONS____);
  this->head = that;

  ae_obj_init(that, AE_INTEGER_);
  that->int_value = 123;

  T(ae_obj_length(this) == 1);

  for (unsigned int ix = 1; ix < 4; ix++) { 
    ae_obj_t * new_head = ALLOC_AE_OBJ;
    ae_obj_init(new_head, AE_INTEGER_);
    new_head->int_value = 123 + ix;

    ae_obj_t * tail = this;
    
    this = CONS(new_head, tail);
    
    T(this != that);
    T(this != new_head);
    T(this->head == new_head);
    T(this->tail == tail);
    T(ae_obj_length(this) == 1 + ix);
  }

  {
    printf("Final len %d\n", ae_obj_length(this));

    ae_obj_each(this, incr_cons_and_each_tests_length);
  
    T(cons_and_each_tests_length == 4);
  }
  
  {
    // For expedienc-of-implementation's sake, we'll check if the list is what
    // it's supposed to be by fwriting it into a string and comparing it to a
    // string constant.
    
    // ae_obj_fwrite does dumb shit with backspace:  
    const char * const strcmp_str = "(126 125 124 123 \b) ";
    
    const size_t buff_len = 1 << 8;
    char * buff = malloc(buff_len);
    
    FILE * stream = fmemopen(buff, buff_len, "w");
    ae_obj_fwrite(this, stream);
    fclose(stream);

    T(strcmp(strcmp_str, buff) == 0);
    free(buff);
  }

  ae_obj_t * doubled = ae_obj_map(this, ae_obj_double);
  puts("Doubled is ");
  ae_obj_write(doubled);
  NL;

  {
    // For expedienc-of-implementation's sake, we'll check if the list is what
    // it's supposed to be by fwriting it into a string and comparing it to a
    // string constant.
    
    // ae_obj_fwrite does dumb shit with backspace:  
    const char * const strcmp_str = "(252 250 248 246 \b) ";
    
    const size_t buff_len = 1 << 8;
    char * buff = malloc(buff_len);
    
    FILE * stream = fmemopen(buff, buff_len, "w");
    ae_obj_fwrite(doubled, stream);
    fclose(stream);

    T(strcmp(strcmp_str, buff) == 0);
    free(buff);
  }
}

void unsafe_move(void) {
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


void push_back(void) {
  SETUP_TEST;

  ae_obj_init(this, AE_CONS____);

  // printf("\nWARNING: Feigning length value at line %d in %s, this needs fixing!", __LINE__, __FILE__);
  // T(ae_obj_length(this) == 1);

  T(ae_obj_length(this) == 0);

  for (unsigned int ix = 1; ix < 4; ix++) { 
    ae_obj_t * new_last = ALLOC_AE_OBJ;
    ae_obj_init(new_last, AE_INTEGER_);
    new_last->int_value = 123 + ix;

    ae_obj_t * tail = this;

    ae_obj_push_back(this, new_last);

    T(ae_obj_length(this) == ix);
  }

  // For expedienc-of-implementation's sake, we'll check if the list is what
  // it's supposed to be by fwriting it into a string and comparing it to a
  // string constant.

  // ae_obj_fwrite does dumb shit with backspace:  
  const char * const strcmp_str = "(124 125 126 \b) ";
  
  const size_t buff_len = 1 << 8;
  char * buff = malloc(buff_len);

  FILE * stream = fmemopen(buff, buff_len, "w");
  ae_obj_fwrite(this, stream);
  fclose(stream);

  /* ae_obj_write(this); */
  /* printf("\nCompare '%s' and '%s'.\n", strcmp_str, buff); */
  /* fflush(stdout); */

  T(strcmp(strcmp_str, buff) == 0);
  
  free(buff);
}

void simple_clone(void) {
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

#define FOR_TEST_FUNS_DO(X)                                                                                                                 \
  X(newly_allocated_ae_obj_is_inside_pool)                                                                                                  \
  X(newly_initialized_ae_obj_has_correct_type_field)                                                                                        \
  X(newly_initialized_ae_obj_has_zeroed_data_fields)                                                                                        \
  X(unsafe_move)                                                                                                                            \
  X(simple_clone)                                                                                                                           \
  X(push_back)                                                                                                                              \
  X(cons_and_each)

#define pair(fun) { #fun, fun },

TEST_LIST = {
  FOR_TEST_FUNS_DO(pair)
  { NULL, NULL }
};
