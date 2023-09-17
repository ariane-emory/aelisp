To Do:
- [ ] Better code for aligning type strs when printing objs.
- [ ] Add a free list allocator and make a pool to hold strings' data.
- [ ] Add rational support to ae_obj_eql.
- [ ] Maybe add addressable nil value?
- [ ] Add macro version of map?
- [ ] Redo/rename the so-called 'byte oriented' puts?
- [ ] Maybe also a vector type of some sort?
- [ ] Review whether or not the values of pointers returned by ae_obj_truth is UB and possibly come up with something better.
- [ ] ... advices?!

Completed:
- [x] Test ae_obj_eql.
- [x] Test ae_obj_truth.
- [x] Add eql.
- [x] _fput methods should return ints.
- [x] Add FOR_EACH macro.
- [x] Add whole-line comments beginning with ;; to lexer.
- [x] Add ae_list_has_member.
- [x] Add ae_list_remove_member.
