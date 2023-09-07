#pragma once

/////////////////////////////////////////////////////////////////////////////////
// List type
////////////////////////////////////////////////////////////////////////////////

typedef struct ae_list_node_t {
  void *                    object;
  struct ae_list_node_t *   tail;
} ae_list_node_t;

typedef ae_list_node_t * ae_list_t;

//------------------------------------------------------------------------------

void               ae_list_init(ae_list_t * const ae_list);
const char * const ae_list_str(const ae_list_t * const ae_list);
ae_list_node_t *   ae_list_append(ae_list_t * const ae_list, void * const object);

void               ae_list_node_init(ae_list_node_t * const ae_list_node);
const char * const ae_list_node_str(const ae_list_node_t * const ae_list_node);
ae_list_node_t *   ae_list_node_append(ae_list_node_t * const ae_list_node, void * const object);
