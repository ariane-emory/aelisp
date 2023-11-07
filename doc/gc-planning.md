Safe files - should not need major revision:
- capture.h: should be fine.
- free_list.h/c: might become obsolete, but if not should be fine in current form.
- jump_return.h: should be fine.
- log.h: should be fine.
- time_funcs.c/h: should be fine.
- write: should be fine.
- grammar files: doing GC during parsing is probably not practical and foregoing it might be tolerable as long as GC happens afterwards.

Average files - likely to need some revision:
- obj.h/c: clone and init would need a GC param.
- pool.h/c: probably would be nice to allocate variably sized objects, which would obsolesce free_list.h/c. Also from/to division.

Unsafe files - likely to need major revision:
- common.h/c: load_file should probably trigger GC after parsing.
- env.h/c: many functions will need a GC param.
- eval.h/c: many functions will need a GC param.
- list.h/c: many functions will need a GC param.
- plist.h/c: many functions will need a GC param.
- tesc.c: obiously many tests will need to be re-formulated.
- All core files: every core function will need a GC param.

Unused files - not presently in use and being ignored for now:
- alist.h/c.
