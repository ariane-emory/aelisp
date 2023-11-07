Safe files - should not need major revision:
- capture.h: should be fine.
- jump_return.h: should be fine.
- log.h/c: should be fine.
- time_funcs.c/h: should be fine.
- write: should be fine.
- free_list.h/c: might become obsolete, but if not should be fine in current form. splitting into from/to spaces could possibly benefit from a void   free_list_remove_block function, undecided.
- grammar files: doing GC during parsing is probably not practical and foregoing it might be tolerable in stage 1 as long as GC happens afterwards. Could be done in stage 2.

Average files - likely to need some revision:
- obj.h/c: clone and init would need a GC param.
- pool.h/c: probably would be nice to allocate variably sized objects, which would obsolesce free_list.h/c. Also from/to division. Either that or replace it entirely with a new pool structure.

Unsafe files - likely to need major revision:
- common.h/c: load_file should probably trigger GC after parsing.
- env.h/c: many functions will need a GC param.
- eval.h/c: many functions will need a GC param.
- list.h/c: somefunctions will need a GC param (e.g. ae_list_cons).
- plist.h/c: many functions will need a GC param.
- tesc.c: obiously many tests will need to be re-formulated.
- All core files: every core function and maybe some helpers will need a GC param.

Unused files - not presently in use and being ignored entirely for now:
- alist.h/c.

Files that could be temporarily excluded to build a minimal test case:
- All core files.
- capture.h
- common.h/c.
- eval.h/c.
- free_list.h/c.
- jump_return.h.
- log.h/c.
- plist.h/c.
- time_funcs.h.

Files that would be used in the minimal test case:
- env.h/c.
- list.h/c.
- obj.h/c.
- pool.h/c (maybe, unless replaced with a new pool structure).

Compromises on initial stages:
- stage 1 will not adress string pool, it could still fill up.
- stage 2 will use a 'from' and 'to' string pool.
- possible stage 3 will revise pool to allocate various sizes and eliminate separate string pool.
