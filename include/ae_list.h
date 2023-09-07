#pragma once

struct ae_object_t;

////////////////////////////////////////////////////////////////////////////////
// List type
////////////////////////////////////////////////////////////////////////////////

typedef struct ae_list_node_t {
  struct ae_object_t *      object;
  struct ae_list_node_t *   tail;
} ae_list_node_t;

typedef ae_list_node_t * ae_list_t;

//------------------------------------------------------------------------------

void               ae_list_init(ae_list_t * const ae_list);
const char * const ae_list_str(const ae_list_t * const ae_list);
void               ae_list_append(ae_list_t * const ae_list, struct ae_object_t * const ae_object);

void               ae_list_node_init(ae_list_node_t * const ae_list_node);
const char * const ae_list_node_str(const ae_list_node_t * const ae_list_node);
void               ae_list_node_append(ae_list_node_t * const ae_list_node, struct ae_object_t * const ae_object);
