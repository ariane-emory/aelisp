To Do:
- [X] Finish adding an addressable nil object.
- [ ] ae_obj_truth(false) and the empty list should be eq and eql but probably isn't right now? Check.
- [ ] Add a free list allocator and make a pool to hold strings' data.
- [ ] Review (and test) the 'word-oriented' puts.
- [ ] Review whether or not the values of pointers returned by ae_obj_truth are UB and possibly come up with something better.
- [ ] Fix inappropriate handling of quote.
- [ ] Maybe add macro version of map?
- [ ] Maybe also a vector type of some sort?
- [ ] ... advices?!

Completed:
- [x] Revise how spaces are inserted between elements when writing lists.
- [x] Add tests for using rationals with ae_obj_eql.
- [x] Add rational support to ae_obj_eql.
- [x] Rename the so called 'byte-oriented' puts (they are actually word-oriented).
- [x] Better code for aligning type strs when printing objs.
- [x] Test ae_obj_eql.
- [x] Test ae_obj_truth.
- [x] Add eql.
- [x] _fput methods should return ints.
- [x] Add FOR_EACH macro.
- [x] Add whole-line comments beginning with ;; to lexer.
- [x] Add ae_list_has_member.
- [x] Add ae_list_remove_member.
