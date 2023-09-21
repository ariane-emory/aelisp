#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#define offsetof(type, member) ((size_t)&((type *)0)->member)
#define ALLOC_HEADER_SIZE      (offsetof(ae_node_t, data))
#define MIN_ALLOC_SIZE         (ALLOC_HEADER_SIZE + 64)
#define uintptr(p)             ((uintptr_t)(p))

#ifndef align_up
#  define align_up(num, align) (((num) + ((align)-1)) & ~((align)-1))
#endif

////////////////////////////////////////////////////////////////////////////////////////////////////
// ae_node_t struct
////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct ae_node_t {
  struct ae_node_t * next;
  struct ae_node_t * prev;
  size_t             size;
  char *             data;
} ae_node_t;

#define AE_NODE_FOR_EACH(pos, head)                                                                \
  for ((pos) =  (head)->next;                                                                      \
       (pos) != (head);                                                                            \
       (pos) =  (pos)->next)

#define AE_NODE_FOR_EACH_SAFE(pos, pos_next, head)                                                 \
  for ((pos) = (head)->next,                                                                       \
         (pos_next) = (pos)->next;                                                                 \
       (pos) != (head);                                                                            \
       (pos) = (pos_next), (pos_next) = (pos)->next)

////////////////////////////////////////////////////////////////////////////////////////////////////
// ae_node_insert
////////////////////////////////////////////////////////////////////////////////////////////////////

static void ae_node_insert(ae_node_t * prev, ae_node_t * this, ae_node_t * next) {
  next->prev = this;
  this->next = next;
  this->prev = prev;
  prev->next = this;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// ae_node_remove
////////////////////////////////////////////////////////////////////////////////////////////////////

static void ae_node_remove(ae_node_t * this) {
  ae_node_t * prev = this->prev;
  ae_node_t * next = this->next;

  next->prev = prev;
  prev->next = next;

  this->next = NULL;
  this->prev = NULL;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// free_list data
////////////////////////////////////////////////////////////////////////////////////////////////////

static ae_node_t free_list = { &free_list, &free_list, 0, NULL };

////////////////////////////////////////////////////////////////////////////////////////////////////
// free_list_add_block
////////////////////////////////////////////////////////////////////////////////////////////////////

void free_list_add_block(void * addr, size_t size) {
  ae_node_t * node;  

  node       = (void *)   align_up(uintptr(addr), sizeof(void *));
  node->size = (uintptr_t)addr + size - uintptr(node) - ALLOC_HEADER_SIZE;

  ae_node_insert(&free_list, node, free_list.next);
}
////////////////////////////////////////////////////////////////////////////////////////////////////
// free_list_malloc
////////////////////////////////////////////////////////////////////////////////////////////////////

void * free_list_malloc(size_t size) {
  void      * ptr  = NULL;
  ae_node_t * node = NULL;

  assert(size != 0);
    
  size = align_up(size, sizeof(void *));
    
  AE_NODE_FOR_EACH(node, &free_list)
    if (node->size >= size) {
      ptr = &node->data;
            
#ifdef AE_LOG_FREE_LIST
      printf("Selected node %p.\n", node);
      printf("With data at  %p.\n", &node->data);
#endif

      break;
    }

  if (ptr) {
    // Maybe split the data
    if ((node->size - size) >= MIN_ALLOC_SIZE) {
      ae_node_t * new_node = (ae_node_t *)(uintptr(&node->data) + size);
      new_node->size       = node->size - size - ALLOC_HEADER_SIZE;
      node->size           = size;

#ifdef AE_LOG_FREE_LIST
      printf("Split         %p.\n", new_node);
#endif

      ae_node_insert(node, new_node, node->next);
    }

    ae_node_remove(node);
  }

#ifdef AE_LOG_FREE_LIST
  printf("Returning     %p.\n", ptr);
#endif

  return ptr;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// free_list_coalesce
////////////////////////////////////////////////////////////////////////////////////////////////////

static void free_list_coalesce(void) {
  ae_node_t * node;
  ae_node_t * node_next;
  ae_node_t * last_node = NULL;

  AE_NODE_FOR_EACH_SAFE(node, node_next, &free_list) {
    if (last_node) {
      if (((uintptr(&last_node->data)) + last_node->size) == uintptr(node)) {
        last_node->size += ALLOC_HEADER_SIZE;
        last_node->size += node->size;

        ae_node_remove(node);

        continue;
      }
    }
    last_node = node;
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// free_list_free
////////////////////////////////////////////////////////////////////////////////////////////////////

void free_list_free(void * ptr) {
#ifdef AE_LOG_FREE_LIST
  printf("free          %p\n", ptr);
#endif

  ae_node_t * node = (ae_node_t *)(uintptr(ptr) - ALLOC_HEADER_SIZE);

#ifdef AE_LOG_FREE_LIST
  printf("node          %p\n", node);
#endif
  
  ae_node_t * free_node;

  AE_NODE_FOR_EACH(free_node, &free_list) {
    if (free_node > node) {
      ae_node_insert(free_node->prev, node, free_node);

      goto end;
    }
  }

  ae_node_insert(free_list.prev, node, &free_list);

end:
  free_list_coalesce();
}
