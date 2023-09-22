#pragma once

#include "ae_obj.h"
#include "ae_obj_list.h"

////////////////////////////////////////////////////////////////////////////////////////////////////
// Typedefs
////////////////////////////////////////////////////////////////////////////////////////////////////

typedef        void       (*ae_list_each_fun)(      struct ae_obj_t * const);
typedef struct ae_obj_t * (*ae_list_map_fun )(const struct ae_obj_t * const);

////////////////////////////////////////////////////////////////////////////////////////////////////
// convenience macros
////////////////////////////////////////////////////////////////////////////////////////////////////
#define CONS_NIL(this)          (CONS((this), NIL))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define CAR(this)               ((this)->head)
#define CDR(this)               ((this)->tail)
#define CAAR(this)              (CAR(CAR(this)))
#define CADR(this)              (CAR(CDR(this)))
#define CDAR(this)              (CDR(CAR(this)))
#define CDDR(this)              (CDR(CDR(this)))
#define CADAR(this)             (CAR(CDR(CAR(this))))
#define CONS(head, tail)        (ae_obj_cons((head), (tail)))
////////////////////////////////////////////////////////////////////////////////////////////////////
#define EACH(this, fun)         (ae_list_each(this, (ae_list_each_fun)fun))
#define MAP(this, fun)          (ae_list_map(this, (ae_list_map_fun)fun))
#define LENGTH(this)            (ae_list_length(this))
#define PUSH(this, that)        (ae_list_push_back((this), (that)))
#define REMOVE(list, elem)      (ae_list_remove_member(list, elem))
#define INTERN2(sym_list, str)  (ae_list_intern_string((sym_list), (str)))
#define INTERN(str)             (INTERN2(&symbols_list, (str)))
///////////////////////////////////////////////////////////////j/////////////////////////////////////
#define TAILP(o)                (NOT_NULLP((o)) && (NILP((o)) || (CONSP((o)) && CAR((o)))))
#define MEMBERP(this, that)     (ae_list_has_member((this), (that)))
#define NOT_MEMBERP(this, that) (! MEMBERP((this), (that)))
#define NOT_TAILP(this, that)   (! TAILP((this), (that))) 
#define ASSERT_TAILP(o)         (assert(TAILP((o))))
///////////////////////////////////////////////////////////////j/////////////////////////////////////
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
ae_obj_t *    ae_obj_cons          (      ae_obj_t *  const head,  ae_obj_t *  const tail         );
ae_obj_t *    ae_list_push_back    (      ae_obj_t ** const plist, ae_obj_t *  const member       );
ae_obj_t *    ae_list_remove_member(      ae_obj_t *  const list,  ae_obj_t *  const member       );
bool          ae_list_has_member   (const ae_obj_t *  const list,  ae_obj_t *  const member       );
int           ae_list_length       (const ae_obj_t *  const list                                  );
ae_obj_t *    ae_list_map          (      ae_obj_t *  const list,  ae_list_map_fun   fun          );
void          ae_list_each         (      ae_obj_t *  const list,  ae_list_each_fun  fun          );
ae_obj_t *    ae_list_intern_string(      ae_obj_t ** const plist, ae_string_t       string       );
////////////////////////////////////////////////////////////////////////////////////////////////////
