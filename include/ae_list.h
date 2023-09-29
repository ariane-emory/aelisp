#pragma once

#include <stdbool.h>

#include "ae_obj.h"
#include "ae_list.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// Typedefs
////////////////////////////////////////////////////////////////////////////////////////////////////

typedef        void       (*ae_list_each_fun)(      struct ae_obj_t * const);
typedef struct ae_obj_t * (*ae_list_map_fun )(const struct ae_obj_t * const);

////////////////////////////////////////////////////////////////////////////////////////////////////
// convenience macros
////////////////////////////////////////////////////////////////////////////////////////////////////
#define LIST(this)                    (CONS((this), NIL))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define CAR(this)                     ((this)->head)
#define CDR(this)                     ((this)->tail)
#define CAAR(this)                    (CAR(CAR(this)))
#define CADR(this)                    (CAR(CDR(this)))
#define CDAR(this)                    (CDR(CAR(this)))
#define CDDR(this)                    (CDR(CDR(this)))
#define CADAR(this)                   (CAR(CDR(CAR(this))))
#define CADDR(this)                   (CAR(CDR(CDR(this))))
#define CONS(head, tail)              (ae_list_cons((head), (tail)))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define EACH(this, fun)               (ae_list_each(this, (ae_list_each_fun)fun))
#define MAP(this, fun)                (ae_list_map(this, (ae_list_map_fun)fun))
#define LENGTH(this)                  (ae_list_length(this))
#define PUSH(this, that)              (ae_list_push_back(&(this), (that)))
#define REMOVE(list, elem)            (ae_list_remove_member(list, elem))
#define SYM2(sym_list, str)           (ae_list_intern_string((sym_list), (str)))
#define SYM(str)                      (SYM2(&symbols_list, (str)))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define TAILP(o)                      ((! NULLP((o))) && (NILP((o)) || (CONSP((o)) && CAR((o)))))
#define MEMBERP(this, that)           (ae_list_has_member((this), (that)))
#define PROPER_LISTP(o)               (ae_list_is_proper((o)))
/////////////////////////////////////////////////////////////////////////////////////////////////////
#define FOR_EACH(elem, list)                                                                       \
  for (ae_obj_t                                                                                    \
         * position = (list),                                                                      \
         * elem     = CAR(position);                                                               \
       CONSP(position);                                                                            \
       elem = CAR(position = CDR(position)))
#define FOR_EACH_CONST(elem, list)                                                                 \
  for (const ae_obj_t                                                                              \
         * position = (list),                                                                      \
         * elem     = CAR(position);                                                               \
       CONSP(position);                                                                            \
       elem = CAR(position = CDR(position)))
////////////////////////////////////////////////////////////////////////////////////////////////////
// Obj's methods list-related methods
////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t *    ae_list_cons          (      ae_obj_t *  const head,  ae_obj_t *  const tail         );
ae_obj_t *    ae_list_push_back    (      ae_obj_t ** const plist, ae_obj_t *  const member       );
ae_obj_t *    ae_list_remove_member(      ae_obj_t *  const list,  ae_obj_t *  const member       );
bool          ae_list_has_member   (const ae_obj_t *  const list,  ae_obj_t *  const member       );
int           ae_list_length       (const ae_obj_t *  const list                                  );
ae_obj_t *    ae_list_map          (      ae_obj_t *  const list,  ae_list_map_fun   fun          );
void          ae_list_each         (      ae_obj_t *  const list,  ae_list_each_fun  fun          );
ae_obj_t *    ae_list_intern_string(      ae_obj_t ** const plist, ae_string_t       string       );
bool          ae_list_is_proper    (const ae_obj_t *  const list                                   );
////////////////////////////////////////////////////////////////////////////////////////////////////
