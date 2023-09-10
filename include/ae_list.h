#pragma once

/////////////////////////////////////////////////////////////////////////////////
// List type
////////////////////////////////////////////////////////////////////////////////

typedef void   (*ae_list_node_each_fun)(void * const);
typedef void * (*ae_list_node_map_fun) (void * const);

typedef struct ae_list_node_t {
  void *                    object;
  struct ae_list_node_t *   tail;
} ae_list_node_t;

typedef ae_list_node_t * ae_list_t;

//------------------------------------------------------------------------------

void               ae_list_init          (      ae_list_t * const this);
size_t             ae_list_length        (const ae_list_t * const this);
ae_list_node_t *   ae_list_push_back     (      ae_list_t * const this, void * const object);
const char *       ae_list_str           (const ae_list_t * const this);
void               ae_list_each          (      ae_list_t * const this, ae_list_node_each_fun fun);
ae_list_t          ae_list_map           (const ae_list_t * const this, ae_list_node_map_fun fun);
void               ae_list_map_into_from (      ae_list_t * const this, const ae_list_t * const that, ae_list_node_map_fun fun);

void               ae_list_node_init     (      ae_list_node_t * const this);
size_t             ae_list_node_length   (const ae_list_node_t * const this);
ae_list_node_t *   ae_list_node_push_back(      ae_list_node_t * const this, void * const object);
const char *       ae_list_node_str      (const ae_list_node_t * const this);
void               ae_list_node_each     (      ae_list_node_t * const this, ae_list_node_each_fun fun);
