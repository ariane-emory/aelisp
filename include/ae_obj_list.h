#pragma once

#include <stdbool.h>

////////////////////////////////////////////////////////////////////////////////////////////////////
// Typedefs
////////////////////////////////////////////////////////////////////////////////////////////////////

typedef void              (*ae_list_each_fun)(struct ae_obj_t *  const);
typedef struct ae_obj_t * (*ae_list_map_fun )(const struct ae_obj_t *  const);

////////////////////////////////////////////////////////////////////////////////////////////////////
// convenience macros
////////////////////////////////////////////////////////////////////////////////////////////////////

#define LENGTH(this)            (ae_list_length(this))
#define MAP(this, fun)          (ae_list_map(this, (ae_list_map_fun)fun))
#define MEMBER(this, that)      (ae_list_has_member((this), (that)))
#define REMOVE(list, elem)      (ae_list_remove_member(list, elem))

#define FOR_EACH(elem, list)                                                                                                                \
  for (ae_obj_t                                                                                                                             \
         * position = (list),                                                                                                               \
         * elem     = CAR((position));                                                                                                      \
       position;                                                                                                                            \
       elem         = (position = CDR(position)) ? CAR(position) : NULL)                                                                                                                              

#define FOR_EACH_CONST(elem, list)                                                                                                          \
  for (const ae_obj_t                                                                                                                       \
         * position = (list),                                                                                                               \
         * elem     = CAR((position));                                                                                                      \
       position;                                                                                                                            \
       elem         = (position = CDR(position)) ? CAR(position) : NULL)                                                                                                                              

////////////////////////////////////////////////////////////////////////////////////////////////////
// Obj's methods list-related methods
////////////////////////////////////////////////////////////////////////////////////////////////////
ae_obj_t *    ae_obj_cons          (      ae_obj_t *  const head,  ae_obj_t *  const tail         );
ae_obj_t *    ae_list_push_back    (      ae_obj_t *  const list,  ae_obj_t *  const member       );
ae_obj_t *    ae_list_remove_member(      ae_obj_t *  const list,  ae_obj_t *  const member       );
bool          ae_list_has_member   (const ae_obj_t *  const list,  ae_obj_t *  const member       );
size_t        ae_list_length       (const ae_obj_t *  const list                                  );
ae_obj_t *    ae_list_map          (const ae_obj_t *  const list,  ae_list_map_fun   fun          );
void          ae_list_each         (      ae_obj_t *  const list,  ae_list_each_fun  fun          );
ae_obj_t *    ae_list_intern_string(      ae_obj_t ** const plist, ae_string_t       string       );
////////////////////////////////////////////////////////////////////////////////////////////////////

#include "ae_obj_write.h"
