;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 'standard library', such as it is:                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; crucial macros, without which nothing else in stdlib will even work:       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! defmacro
 (macro (name params . body)
  $('setq! name $('macro params . body))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defun (name params . body)
 $('setq! name $('lambda params . body)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type predicates:                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun type?     (typ o) (eq? typ       (type           o)))
(defun atom?     (o)     (not (type?    :CONS           o)))
(defun char?     (o)          (type?    :CHAR           o))
(defun cons?     (o)          (type?    :CONS           o))
(defun core?     (o)          (type?    :CORE           o))
(defun env?      (o)          (type?    :ENV            o))
(defun error?    (o)          (type?    :ERROR          o))
(defun float?    (o)          (type?    :FLOAT          o))
(defun integer?  (o)          (type?    :INTEGER        o))
(defun lambda?   (o)          (type?    :LAMBDA         o))
(defun λ?        (o)          (type?    :LAMBDA         o))
(defun macro?    (o)          (type?    :MACRO          o))
(defun rational? (o)          (type?    :RATIONAL       o))
(defun string?   (o)          (type?    :STRING         o))
(defun symbol?   (o)          (type?    :SYMBOL         o))
(defun improper? (o)     (and (tail? o) (not (proper?   o))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compound car/cdrs:                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun caar     (lst)               (car (car lst)))
(defun cadr     (lst)               (car (cdr lst)))
(defun cdar     (lst)               (cdr (car lst)))
(defun cddr     (lst)               (cdr (cdr lst)))
(defun caaar    (lst)          (car (car (car lst))))
(defun caadr    (lst)          (car (car (cdr lst))))
(defun cadar    (lst)          (car (cdr (car lst))))
(defun caddr    (lst)          (car (cdr (cdr lst))))
(defun cdaar    (lst)          (cdr (car (car lst))))
(defun cdadr    (lst)          (cdr (car (cdr lst))))
(defun cddar    (lst)          (cdr (cdr (car lst))))
(defun cdddr    (lst)          (cdr (cdr (cdr lst))))
(defun caaaar   (lst)     (car (car (car (car lst)))))
(defun caaadr   (lst)     (car (car (car (cdr lst)))))
(defun caadar   (lst)     (car (car (cdr (car lst)))))
(defun caaddr   (lst)     (car (car (cdr (cdr lst)))))
(defun cadaar   (lst)     (car (cdr (car (car lst)))))
(defun cadadr   (lst)     (car (cdr (car (cdr lst)))))
(defun caddar   (lst)     (car (cdr (cdr (car lst)))))
(defun cadddr   (lst)     (car (cdr (cdr (cdr lst)))))
(defun cdaaar   (lst)     (cdr (car (car (car lst)))))
(defun cdaadr   (lst)     (cdr (car (car (cdr lst)))))
(defun cdadar   (lst)     (cdr (car (cdr (car lst)))))
(defun cdaddr   (lst)     (cdr (car (cdr (cdr lst)))))
(defun cddaar   (lst)     (cdr (cdr (car (car lst)))))
(defun cddadr   (lst)     (cdr (cdr (car (cdr lst)))))
(defun cdddar   (lst)     (cdr (cdr (cdr (car lst)))))
(defun cddddr   (lst)     (cdr (cdr (cdr (cdr lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun first    (lst)                    (car lst))
(defun second   (lst)                   (cadr lst))
(defun third    (lst)                  (caddr lst))
(defun fourth   (lst)                 (cadddr lst))
(defun fifth    (lst)            (car (cddddr lst)))
(defun sixth    (lst)           (cadr (cddddr lst)))
(defun seventh  (lst)          (caddr (cddddr lst)))
(defun eighth   (lst)         (cadddr (cddddr lst)))
(defun ninth    (lst)    (car (cddddr (cddddr lst))))
(defun tenth    (lst)   (cadr (cddddr (cddddr lst))))
(defun eleventh (lst)  (caddr (cddddr (cddddr lst))))
(defun twelfth  (lst) (cadddr (cddddr (cddddr lst))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun nth (index lst)
 "Get the nth item in a list."
 (cond
  ((== 0 index) (car lst))
  (lst          (nth (- index 1) (cdr lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun last
 (lst)
 "Get last item in a list."
 (cond
  ((nil? (cdr lst)) lst)
  (lst              (last (cdr lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; quasiquotation                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! append2
 (lambda (lst1 lst2)
  "Append two lists."
  (if (nil? lst1)
   lst2
   (cons (car lst1) (append2 (cdr lst1) lst2)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro expand-quasiquoted (expr)
 "Expand a quasiquoted expression and resolve unquotes and"
 "unquote-splicings within."
 (cond
  ;; If it's not a cons then it's an atom that we should quote.
  ((atom? expr)
   $('quote expr))
  ;; Directly replace (unquote x) with x.
  ((eq? (car expr) 'unquote)
   (car (cdr expr)))
  ;; If the second element of the list is an unquote-splicing, we want to use
  ;; append2.
  ((and
    (cons? (cdr expr))
    (cons? (cadr expr))
    (eq?   (caadr expr) 'unquote-splicing))
   $('append2
     $('list $('expand-quasiquoted (car expr)))
     (cadadr expr)))
  ;; If the second element of the list is an unquote, use cons but without
  ;; splicing.
  ((and
    (cons? (cdr expr))
    (eq?   (cadr expr) 'unquote))
   $('cons
     $('expand-quasiquoted (car expr))
     (caddr expr)))
  ;; Error out for splicing outside of list context
  ((eq? (car expr) 'unquote-splicing)
   (error "unquote-splicing can't occur at top level"))
  ;; If the list is regular, we just recurse on both its parts
  (t $('cons
       $('expand-quasiquoted (car expr))
       $('expand-quasiquoted (cdr expr))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! quasiquote expand-quasiquoted)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (tail chasers):                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro make-chase-fun (pred? . cond-clauses)
  "Create a function to traverse a list based on a predicate.
  
  This macro generates a function that accepts two parameters:
  - A value to search or operate on.
  - A list to process.

  The generated function uses an inner recursive function `chase`
  to traverse the list and apply the conditions from `cond-clauses`.

  Suitable for operations like searching, indexing, or removing items
  from a list based on the provided predicate.

  Use this when:
  - You want to process a list based on a value (like searching for a value).
  - The resulting function needs only the list and a target value.
  - Additional accumulator or state variables aren't needed during recursion."
  `(lambda (x lst . rest)
     (letrec
         ((chase
           (lambda (lst . rest)
            (cond
             ,@cond-clauses))))
       (chase lst . rest))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro make-remove-fun (pred?)
 `(make-chase-fun ,pred?
   ((,pred? (car lst) x) (cdr lst))
   (lst (cons (car lst) (chase (cdr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro make-member-pred (pred?)
 `(make-chase-fun ,pred?
   ((,pred? x (car lst)) t)
   (lst (chase (cdr lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro make-index-fun (pred?)
 `(make-chase-fun ,pred?
   ((nil? lst) nil)
   ((,pred? x (car lst)) (car rest))
   (t (chase (cdr lst) (if rest (1+ (car rest)) 1)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! indexq   (make-index-fun   eq?))
(setq! memq?    (make-member-pred eq?))
(setq! removeq  (make-remove-fun  eq?))
(setq! indexql  (make-index-fun   eql?))
(setq! memql?   (make-member-pred eql?))
(setq! removeql (make-remove-fun  eql?))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (reduction):                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun reduce (fun acc lst)
 "Left reduce (fold) list by applying fun to successive pairs."
 (if (nil? lst)
  acc
  (reduce fun (fun acc (car lst)) (cdr lst))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun reduced (fun)
 "Return a function that is a reduction of the binary function fun."
 (lambda args
  (reduce fun (car args) (cdr args))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rreduce (fun acc lst)
 "Right reduce ('foldr') list by applying fun to successive pairs."
 (if (nil? lst)
  acc
  (fun (car lst) (rreduce fun acc (cdr lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (map variants):                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mapcar (fun lst)
 "Map fun over list, returning the resulting list."
 (when lst
  (cons (fun (car lst)) (mapcar fun (cdr lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mapcar! (fun lst)
 "Map fun over list, altering the list."
 (when lst
  (rplaca! lst (fun (car lst)))
  (mapcar! fun (cdr lst))
  lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mapc (fun lst)
 "Map fun over list for side-effects only, ignoring the results and returning";)
 "nil."
 (when lst
  (fun (car lst))
  (mapc fun (cdr lst))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mapconcat (fun lst delimiter)
 "Map fun over list, returning the result of concatenating the resulting"
 "strings."
 (if lst
  (reduce
   (lambda (acc item)
    (concat acc delimiter item))
   (fun (car lst))
   (mapcar fun (cdr lst)))
  ""))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mapcan (fun lst)
 "Map fun over list and concatenate the results by altering them."
 (when lst
  (let ((result (fun (car lst)))
        (rest   (mapcan fun (cdr lst))))
   (if result
    (nconc! result rest)
    rest))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (append/nconc variants):                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! append (reduced append2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun nconc2! (lst1 lst2)
 "Destructively join two lists."
 (cond
  ((nil? lst1) lst2)
  (t           (rplacd! (last lst1) lst2) lst1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! nconc! (reduced nconc2!))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (push/push-back):                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun push-back! (lst elem)
 "Destructively push elem onto the end of lst."
 (rplacd! (last lst) (cons elem nil))
 lst)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun push! (elem lst)
 "Destructively push elem onto the front of lst."
 (let ((old-car (car lst)))
  (rplaca! lst elem)
  (rplacd! lst (cons old-car (cdr lst)))
  lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun push-back (lst elem)
 "Non-destructively push elem onto the end of lst."
 (append lst (cons elem nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun push (elem lst)
 "Non-destructively push elem onto the front of lst, aka cons."
 (cons elem lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (flattening):                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flatten1 (lst)
 (cond
  ((nil? lst) nil)
  ((tail? (car lst))
   (append (car lst) (flatten1 (cdr lst))))
  (t (cons (car lst) (flatten1 (cdr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flatten-left (lst)
 "Flatten left-nested list structures."
 (if (cons? (car lst))
  (append (flatten-left (car lst)) $(cadr lst))
  lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flatten (lst)
 (when lst
  (if (cons? (car lst))
   (append (flatten (car lst)) (flatten (cdr lst)))
   (cons (car lst) (flatten (cdr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (zipping):                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun zip2 (lst1 lst2)
 "Zip two lists."
 (cond
  ((∨ (nil? lst1) (nil? lst2)) nil)
  (t  (cons  $((car lst1) (car lst2))
       (zip2   (cdr lst1) (cdr lst2))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun zip3 (l1 l2 l3)
 "Zip three lists."
 (mapcar flatten1 (reduce zip2 l1 $(l2 l3))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! left-nested-zip (reduced zip2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro zip lists
 "Zip many lists. This might not flatten properly if the zipped elements are"
 "themselves lists."
 (if (cdr lists)
  `(mapcar flatten (left-nested-zip ,@lists))
  `(mapcar list   ,(car lists))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (transform):                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun transform! (pred? fun obj)
 "Destructively transform the cons tree obj by replacing members matching"
 "pred? with the result of applying fun to them."
 (if (atom? obj)
  (error "obj must be a list")
  (cond
   ((pred? obj) (set! obj (fun obj)))
   ((cons? obj)
    (let ((head (car obj))
          (tail (cdr obj)))
     (cond
      ((pred? head) (rplaca! obj (fun head)))
      ((cons? head) (transform! pred? fun head)))
     (cond
      ((pred? tail) (rplacd! obj (fun tail)))
      ((cons? tail) (rplacd! obj (transform! pred? fun tail))))))
   (t obj))
  obj))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun transform (pred? fun obj)
 "Transform obj by replacing members matching pred? with the result of"
 "applying fun to them or, if obj is not a cons tree, by applying fun to"
 "obj."
 (cond
  ((and (atom? obj) (pred? obj)) (fun obj))
  ((atom? obj) obj)
  (t
   (cons
    (transform pred? fun (car obj))
    (transform pred? fun (cdr obj))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prefetch (expr)
 "Try to optimize expr by replacing it's symbol? members with the result of"
 "looking them up. This is, mysteriously, not a very effective optimization."
 (transform!
  (lambda (x) (and (symbol? x) (bound? x)))
  (lambda (x) (eval x))
  expr))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (vector-style list API):                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-list (size init-val)
 (cond
  ((== 0 size)  nil)
  (t            (cons init-val (make-list (- size 1) init-val)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro make-chase-list-fun (pred? . cond-clauses)
  "Create a function to traverse and manipulate a list based on an index or condition.
  
  This macro generates a function that accepts at least two parameters:
  - A list to process.
  - An index or condition to determine the operation.
  - Optionally, other arguments for operations (e.g., a replacement value).

  The generated function uses an inner recursive function `chase`
  to traverse the list and apply the conditions from `cond-clauses`.

  Suitable for operations that involve manipulating lists in a vector-style
  (like setting a value at a specific index or retrieving a value).

  Use this when:
  - You want to process a list based on an index or another list-based condition.
  - The resulting function may need more than two arguments.
  - You need to maintain state (like an accumulator) across recursive calls."
  `(lambda (lst x . rest)
     (letrec
         ((chase
           (lambda (lst x . rest)
            (cond
             ,@cond-clauses))))
       (chase lst x . rest))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! list-set!
  (make-chase-list-fun ==
   ((nil? lst) (error "list-set! out of range"))
   ((== x 0) (rplaca! lst (car rest)))
   (t (chase (cdr lst) (- x 1) (car rest)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! list-ref
  (make-chase-list-fun ==
   ((nil? lst) (error "list-ref out of range"))
   ((== x 0) (car lst))
   (t (chase (cdr lst) (- x 1)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! list-length length)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (sorting):                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sort (predicate lst)
 "Just a basic merge sort."
 (if (or (not lst) (not (cdr lst))) ; If list has 0 or 1 element
  lst                          ; it's already sorted
  (let ((middle (half lst)))
   (merge (sort predicate (car middle))
    (sort predicate (cdr middle))
    predicate))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun half (lst)
 "Splits lst into two approximately equal halves."
 (let ((slow lst)
       (fast (cdr lst)))
  (while (and fast (cdr fast))
   (setq! slow (cdr slow))
   (setq! fast (cdr (cdr fast))))
  (let ((second-half (cdr slow)))
   (rplacd! slow nil) ; Split the list
   (cons lst second-half))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun merge (predicate lst1 lst2)
 "Merges two sorted lists based on predicate."
 (cond
  ((not lst1) lst2)
  ((not lst2) lst1)
  ((predicate (car lst1) (car lst2))
   (cons (car lst1) (merge predicate (cdr lst1) lst2)))
  (t                                                                          ;;
   (cons (car lst2) (merge predicate lst1 (cdr lst2))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (unsorted):                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun list* args
 (let*
  ((chase
	  (lambda (args)
		 (cond
      ((nil? args)       nil)
		  ((nil? (cdr args)) (car args))
		  (t                 (cons (car args) (chase (cdr args))))))))
  (chase args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun depth (lst)
 "Get the depth of a nested list structure. This one is untested."
 (if (atom? lst)
  0
  (max 1 (+ (depth (car lst)) (depth (cdr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun filter (pred? lst)
 "Return a list containing those members of lst satisfying pred?."
 (cond
  ((nil? lst) nil)
  ((pred? (car lst))
   (cons (car lst) (filter pred? (cdr lst))))
  (t (filter pred? (cdr lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun intercalate (intercalated lst)
 "Intercalate intercalated between items."
 (if (∨ (nil? lst) (nil? (cdr lst)))
  lst
  (cons (car lst)
   (cons intercalated
    (intercalate intercalated (cdr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (more unsorted):                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defun-list-pred-fun (name combiner base-case)
 `(defun ,name (pred? lst)
   (if lst
    (,combiner
     (pred? (car lst))
     (,name pred? (cdr lst)))
    ,base-case)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defun-list-transform-fun (name transformer)
 `(defun ,name (lsts)
   (when lsts
    (cons (,transformer (car lsts)) (,name (cdr lsts))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun-list-pred-fun any? or  nil)
(defun-list-pred-fun all? and t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun-list-transform-fun heads caar)
(defun-list-transform-fun tails cdar)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (unions):                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun union2 (memp? list1 list2)
 "Return the union of two lists."
 (let* ((combine (lambda (acc x) (if (memp? x acc) acc (cons x acc))))
        (union1 (reduce combine '() list1)))
  (reduce combine union1 list2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! union2q
 (lambda (lst1 lst2)
  (union2 memq? lst1 lst2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! unionq
 (reduced union2q))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! union2ql
 (lambda (lst1 lst2)
  (union2 memq? lst1 lst2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! unionql
 (reduced union2ql))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; log toggle helpers, these should be replaced with macros:                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun with-toggled-fun1 (toggled-fun)
 "Get a function that enables toggled-fun, evaluates fun-or-expr and sets"
 "toggled-fun back to it's prior state."
 (lambda (fun-or-expr)
  (if (lambda? fun-or-expr)
   (let* ((old    (toggled-fun t))
          (result (fun-or-expr))
          (new    (toggled-fun old)))
    result)
   (let* ((old    (toggled-fun t))
          (result (eval fun-or-expr))
          (new    (toggled-fun old)))
    result))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun with-toggled-fun (toggled-fun)
 "Get a function that enables toggled-fun, evaluates fun-or-exprs and sets"
 "toggled-fun back to it's prior state."
 (lambda funs-or-exprs
  (last (mapcar (with-toggled-fun1 toggled-fun) funs-or-exprs))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! with-log-eval (with-toggled-fun log-eval))
(setq! with-log-core (with-toggled-fun log-core))
(setq! with-log-all  (with-toggled-fun log-all))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fancy output funs:                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defmacro princn args       $(progn (cons princ args)                 $(nl)))
;; (defmacro printn args       $(progn (cons print args)                 $(nl)))
;; (defmacro putn   args       $(progn (cons put   args)                 $(nl)))
;; (defmacro writen args       $(progn (cons write args)                 $(nl)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun princn  args       (apply princ   args) (nl))
(defun printn  args       (apply print   args) (nl))
(defun putn    args       (apply put     args) (nl))
(defun writen  args       (apply write   args) (nl))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun princni (i . args) (apply princn  (intercalate i args)))
(defun printni (i . args) (apply printn  (intercalate i args)))
(defun putni   (i . args) (apply putn    (intercalate i args)))
(defun writeni (i . args) (apply writen  (intercalate i args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun princns args       (apply princni (cons " " args)))
(defun printns args       (apply printni (cons " " args)))
(defun putns   args       (apply putni   (cons " " args)))
(defun writens args       (apply writeni (cons " " args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; equal? predicate:                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun equal? (o1 o2)
 "True when o1 and o2 are eql? or cons trees whose atomic mebers are equal?."
 (cond
  ((and (atom? o1) (atom? o2)) (eql? o1 o2))
  ((and (cons? o1) (cons? o2))
   (and (equal? (car o1) (car o2))
    (equal? (cdr o1) (cdr o2))))
  (t nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; even?/odd? predicates:                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun even?   (n) "t if n is even." (== 0 (% n 2 )))
(defun odd?    (n) "t if n is odd."  (== 1 (% n 2 )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; always?/never? predicates:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun always? (x) "always true." t)
(defun never?  (x) "nevertrue."   nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; manipulate predicates:                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun compose-pred1 preds
 "Does what it says on the tin and composes unary predicatess."
 (lambda (val)
  (lets
   ((fun
     (lambda (preds)
      (cond
       ((nil? (car preds))       t)
       ((not  ((car preds) val)) nil)
       (t     (fun (cdr preds)))))))
   (fun preds))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun invert-pred1 pred?
 "Does what it says on the tin and inverts a unary predicate preds."
 (lambda (val)
  (not (pred? val))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; random unsorted stuff:                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun apply* (proc . args)
 (apply proc (apply list* args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun id     (o)   o)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun max    (a b) (if (> a b) a b))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun min    (a b) (if (< a b) a b))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun 1+     (n)   (+ 1 n))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun 1-     (n)   (- n 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun double (n)   (<< n 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! 2*     double)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun benchmark (repetitions print-interval qexpr)
 "Benchmark expr by running it repetitions time and returning the"
 "total/average time in ms, printing updates ever print-interval iterations."
 (nl)
 (let ((ctr   0)
       (total 0))
  (repeat repetitions
   (setq! ctr (1+ ctr))
   (let ((before (time)))
    (eval qexpr)
    (setq! total (+ total (elapsed before))))
   (when (== 0 (% ctr print-interval))
    (nl)
    (princ "Iteration #")
    (princ ctr)
    (princ ", ")
    (princ (÷ total 1000))
    (princ " ms so far.")))
  (nl)
  (princ "total ums: ")
  (princ total) (nl)
  (princ "total ms: ")
  (princ (÷ total 1000)) (nl)
  (princ "total s: ")
  (princ (÷ total 1000000)) (nl)
  (let ((each-ms (÷ total repetitions 1000)))
   (princ "each ms: ")
   (princ (÷ total repetitions 1000))
   (nl)
   each-ms)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; tiny-clos scheme compat:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when nil?
 (setq! #f            nil)
 (setq! #t            t)
 (setq! ???           'unspecified-result)
 (setq! assoc         ahas) 
 (setq! assq          aget) 
 (setq! collect-if    filter)
 (setq! define        setq!)
 (setq! display       write) ;should should be a macro that avoids re-defining what-scheme-implementation
 (setq! else          t)
 (setq! every         all?)
 (defun funcall args  (eval args))
 (setq! getl          pget)
 (setq! gsort         sort)
 (setq! make-vector   make-list)
 (setq! map           mapcar)
 (setq! map-append    mapcan)
 (setq! null?         nil?)
 (setq! position-of   indexq)
 (setq! remove        removeq)
 (setq! set!          setq!)
 (setq! vector-length list-length)
 (setq! vector-ref    list-ref)
 (setq! vector-set!   list-set!))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
