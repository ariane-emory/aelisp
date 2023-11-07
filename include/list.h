#pragma once

#include <stdbool.h>

#include "obj.h"
#include "list.h"

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Typedefs
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef        void       (*ae_list_each_fun)(      struct ae_obj_t * const);
typedef struct ae_obj_t * (*ae_list_map_fun )(const struct ae_obj_t * const);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// convenience macros
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define CAR(list)                     ((list)->head)
#define CDR(list)                     ((list)->tail)
//----------------------------------------------------------------------------------------------------------------------
#define CAAR(list)                    (CAR(CAR(list)))
#define CADR(list)                    (CAR(CDR(list)))
#define CDAR(list)                    (CDR(CAR(list)))
#define CDDR(list)                    (CDR(CDR(list)))
//----------------------------------------------------------------------------------------------------------------------
#define CAAAR(list)                   (CAR(CAR(CAR(list))))
#define CAADR(list)                   (CAR(CAR(CDR(list))))
#define CADAR(list)                   (CAR(CDR(CAR(list))))
#define CADDR(list)                   (CAR(CDR(CDR(list))))
#define CDAAR(list)                   (CDR(CAR(CAR(list))))
#define CDADR(list)                   (CDR(CAR(CDR(list))))
#define CDDAR(list)                   (CDR(CDR(CAR(list))))
#define CDDDR(list)                   (CDR(CDR(CDR(list))))
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define CONS(head, tail)              (ae_list_cons((head), (tail)))
//      ^ this only conses onto tails that are TAILP. To create improper lists, use NEW_CONS instead.
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#ifdef AE_LIST_EACH_AND_MAP
#  define EACH(list, fun)               (ae_list_each((list), (ae_list_each_fun)fun))
#  define MAP(list, fun)                (ae_list_map((list), (ae_list_map_fun)fun))
#endif
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define LENGTH(list)                  (ae_list_length((list)))
#define POP(list)                     (ae_list_pop(&(list)))
#define PUSH(elem, list)              (ae_list_push(&(list), (elem)))
#define PUSH_BACK(list, elem)         (ae_list_push_back(&(list), (elem)))
#define SYM(str)                      (SYM2(&symbols_list, (str)))
#define SYM2(sym_list, str)           (ae_list_intern_string((sym_list), (str)))
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#ifdef AE_LIST_REMOVE
#  define REMOVE(list, elem)          (ae_list_remove_member((list), elem))
#endif
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define MEMBERP(list, elem)           (ae_list_has_member((list), (elem)))
#define PROPERP(obj)                  (ae_list_is_proper((obj)))
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH(elem, list)                                                                                           \
  for (ae_obj_t                                                                                                        \
         * position = (list),                                                                                          \
         * elem     = CAR(position);                                                                                   \
       CONSP(position);                                                                                                \
       elem = CAR(position = CDR(position)))
#define FOR_EACH_CONST(elem, list)                                                                                     \
  for (const ae_obj_t                                                                                                  \
         * position = (list),                                                                                          \
         * elem     = CAR(position);                                                                                   \
       CONSP(position);                                                                                                \
       elem = CAR(position = CDR(position)))
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Obj's methods list-related methods
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t *    ae_list_cons         (      ae_obj_t *  const head,  ae_obj_t   * const tail                            );
ae_obj_t *    ae_list_intern_string(      ae_obj_t ** const plist, const char * const string                          );
ae_obj_t *    ae_list_join3        (      ae_obj_t *        front, ae_obj_t   * const middle, ae_obj_t * const back   );
bool          ae_list_has_member   (const ae_obj_t *  const list,  ae_obj_t   * const member                          );
bool          ae_list_is_proper    (const ae_obj_t *  const list                                                      );
int           ae_list_length       (const ae_obj_t *  const list                                                      );
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// push/pop, these 2 are only used by:
//   ENV_PUSH in ae_common_new_root
//   filename_stack / line_stack in load_file
//   tests
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t *    ae_list_pop          (      ae_obj_t ** const plist                                                     );
ae_obj_t *    ae_list_push         (      ae_obj_t ** const plist, ae_obj_t *  const  member                          );
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// push_back, this is only used by:
//   ae_list_remove_member
//   ae_list_map
//   tests 
ae_obj_t *    ae_list_push_back    (      ae_obj_t ** const plist, ae_obj_t *  const  member                          );
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Everything that follows is currently disabled:
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#ifdef AE_LIST_REMOVE
ae_obj_t *    ae_list_remove_member(      ae_obj_t *  const list,  ae_obj_t *  const  member                          );
#endif
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#ifdef AE_LIST_EACH_AND_MAP
ae_obj_t *    ae_list_map          (      ae_obj_t *  const list,  ae_list_map_fun    fun                             );
void          ae_list_each         (      ae_obj_t *  const list,  ae_list_each_fun   fun                             );
#endif
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
