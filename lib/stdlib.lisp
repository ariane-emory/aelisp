;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; 'standard library', such as it is:                                         ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; crucial macros, without which nothing else in stdlib will even work:       ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! defmacro
 (macro (name params . body)
  (list 'setq! name (list 'macro params . body))))

(defmacro defun (name params . body)
 (list (quote setq!) name (list 'lambda params . body)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; type predicates:                                                           ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun type?     (typ o) (eq?   typ    (type            o                    )))
(defun atom?     (o)     (not? (type?   :CONS           o                    )))
(defun char?     (o)           (type?   :CHAR           o                     ))
(defun cons?     (o)           (type?   :CONS           o                     ))
(defun core?     (o)           (type?   :CORE           o                     ))
(defun env?      (o)           (type?   :ENV            o                     ))
(defun error?    (o)           (type?   :ERROR          o                     ))
(defun float?    (o)           (type?   :FLOAT          o                     ))
(defun integer?  (o)           (type?   :INTEGER        o                     ))
(defun lambda?   (o)           (type?   :LAMBDA         o                     ))
(defun macro?    (o)           (type?   :MACRO          o                     ))
(defun rational? (o)           (type?   :RATIONAL       o                     ))
(defun string?   (o)           (type?   :STRING         o                     ))
(defun symbol?   (o)           (type?   :SYMBOL         o                     ))
(defun improper? (o)     (and? (tail? o) (not? (proper? o                   ))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; fancy output funs which should be replaced with macros:                    ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun princn    args (apply princ args)                                   (nl))
(defun printn    args (apply print args)                                   (nl))
(defun putn      args (apply put   args)                                   (nl))
(defun writen    args (apply write args)                                   (nl))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun princns   args (apply princ (intercalate " " args))                 (nl))
(defun printns   args (apply print (intercalate " " args))                 (nl))
(defun putns     args (apply put   (intercalate " " args))                 (nl))
(defun writens   args (apply write (intercalate " " args))                 (nl))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun princni   (i . args) (apply princ (intercalate i args))             (nl))
(defun printni   (i . args) (apply print (intercalate i args))             (nl))
(defun putni     (i . args) (apply put   (intercalate i args))             (nl))
(defun writeni   (i . args) (apply write (intercalate i args))             (nl))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; compound car/cdrs:                                                         ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun caar      (x)             (car (car x)))
(defun cadr      (x)             (car (cdr x)))
(defun cdar      (x)             (cdr (car x)))
(defun cddr      (x)             (cdr (cdr x)))
(defun caaar     (x)        (car (car (car x))))
(defun caadr     (x)        (car (car (cdr x))))
(defun cadar     (x)        (car (cdr (car x))))
(defun caddr     (x)        (car (cdr (cdr x))))
(defun cdaar     (x)        (cdr (car (car x))))
(defun cdadr     (x)        (cdr (car (cdr x))))
(defun cddar     (x)        (cdr (cdr (car x))))
(defun cdddr     (x)        (cdr (cdr (cdr x))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; equal? predicate:                                                          ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun equal? (o1 o2)
 "True when o1 and o2 are eql? or cons trees whose atomic members are equal?."
 (cond
  ((and? (atom? o1) (atom? o2)) (eql? o1 o2))
  ((and? (cons? o1) (cons? o2))
   (and? (equal? (car o1) (car o2))
    (equal? (cdr o1) (cdr o2))))
  (t nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; even?/odd? predicates:                                                     ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun even?     (n)        (== 0 (% n 2 )))
(defun odd?      (n)        (== 1 (% n 2 )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; list funs (map variants):                                                  ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun mapcar (fun lst)
 "Map fun over list, returning the resulting list."
 (if (nil? lst)
  nil
  (cons (fun (car lst)) (mapcar fun (cdr lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun mapc (fun lst)
 "Map fun over list for side-effects only, ignoring the results and returning nil."
 (if (nil? lst)
  nil
  (fun (car lst))
  (mapc fun (cdr lst))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun mapconcat (fun lst delimiter)
 "Map fun over list, returning the result of concatenating the resulting strings."
 (if (nil? lst)
  ""
  (reduce
   (lambda (acc item)
    (concat acc delimiter item))
   (fun (car lst))
   (mapcar fun (cdr lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun mapcan (fun lst)
 "Map fun over list, returning the result of appending the resulting lists."
 (if (nil? lst)
  nil
  (let ((result (fun (car lst)))
        (rest   (mapcan fun (cdr lst))))
   (if (nil? result)
    rest
    (nconc! result rest)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; list funs (nth/last):                                                      ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun nth (n lst)
 "Get nth item in a list."
 (cond
  ((nil? lst)  nil)
  ((eql? n 0)  (car lst))
  (t           (nth (- n 1) (cdr lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun last
 (lst)
 "Get last item in a list."
 (cond
  ((nil? lst) nil)
  ((nil? (cdr lst)) lst)
  (t (last (cdr lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; reduced:                                                                   ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun reduced (fun)
 "Return a function that is a reduction of the binary function fun."
 (lambda args
  (reduce fun (car args) (cdr args))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; list funs (append/nconc variants):                                         ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun append2 (lst1 lst2)
 "Append two lists."
 (if (nil? lst1)
  lst2
  (cons (car lst1) (append2 (cdr lst1) lst2))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! append (reduced append2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun nconc2! (lst1 lst2)
 "Destructively join two lists."
 (cond
  ((nil? lst1) lst2)
  (t (rplacd! (last lst1) lst2)
   lst1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! nconc! (reduced nconc2!))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; list funs (push/push-back):                                                ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun push-back! (lst elem)
 "Destructively push elem onto the end of lst."
 (rplacd! (last lst) (cons elem nil))
 lst)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun push! (elem lst)
 "Destructively push elem onto the front of lst."
 (let ((old-car (car lst)))
  (rplaca! lst elem)
  (rplacd! lst (cons old-car (cdr lst)))
  lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun push-back (lst elem)
 "Non-destructively push elem onto the end of lst."
 (append lst (cons elem nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun push (elem lst)
 "Non-destructively push elem onto the front of lst, aka cons."
 (cons elem lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; list funs (reduce/rreduce/filter):                                         ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun reduce (fun acc lst)
 "Left reduce ('fold') list by applying fun to successive pairs."
 (if (nil? lst)
  acc
  (reduce fun (fun acc (car lst)) (cdr lst))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun rreduce (fun acc lst)
 "Right reduce ('foldr') list by applying fun to successive pairs."
 (if (nil? lst)
  acc
  (fun (car lst) (rreduce fun acc (cdr lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun filter (pred lst)
 "Return a list containing those members of lst satisfying pred."
 (cond
  ((nil? lst) nil)
  ((pred (car lst))
   (cons (car lst) (filter pred (cdr lst))))
  (t (filter pred (cdr lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; list funs (zipping):                                                       ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun zip2 (lst1 lst2)
 "Zip two lists."
 (cond
  ((or? (nil? lst1) (nil? lst2)) nil)
  (t (cons (list (car lst1) (car lst2))
      (zip2 (cdr lst1) (cdr lst2))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun zip3 (l1 l2 l3)
 "Zip three lists."
 (mapcar flatten1 (reduce zip2 l1 (list l2 l3))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun flatten1 (lst)
 (cond
  ((nil? lst) nil)
  ((tail? (car lst))
   (append (car lst) (flatten1 (cdr lst))))
  (t (cons (car lst) (flatten1 (cdr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun flatten-left (lst)
 "Flatten left-nested list structures."
 (if (cons? (car lst))
  (append (flatten-left (car lst)) (list (cadr lst)))
  lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun flatten (lst)
  (when (not? (nil? lst))
   (if (cons? (car lst))
    (append (flatten (car lst)) (flatten (cdr lst)))
    (cons (car lst) (flatten (cdr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defmacro zip lists
 "Zip many lists."
 (if (cdr lists)
  (list 'mapcar 'flatten (cons 'left-nested-zip lists))
  (list 'mapcar 'list (car lists))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; list funs (transform);                                                     ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun transform! (obj pred fun)
 "Destructively transform the cons tree obj by replacing members matching pred with the result of applying fun to them."
 (if (atom? obj)
  (error "obj must be a list")
  (cond
   ((pred obj) (set! obj (fun obj)))
   ((cons? obj)
    (let ((head (car obj))
          (tail (cdr obj)))
     (cond
      ((pred head)  (rplaca! obj (fun head)))
      ((cons? head) (transform! head pred fun)))
     (cond
      ((pred tail)  (rplacd! obj (fun tail)))
      ((cons? tail) (rplacd! obj (transform! tail pred fun))))))
   (t obj))
  obj))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun transform (obj pred fun)
 "Transform obj by replacing members matching pred with the result of applying fun to them or applying fun to obj if it is not a cons tree."
 (if (atom? obj)
  (if (pred obj)
   (fun obj)
   obj)
  (cons
   (transform (car obj) pred fun)
   (transform (cdr obj) pred fun))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prefetch (expr)
 "Try to optimize expr by replacing it's symbol? members with the result of looking them up. This is, mysteriously, not a very effective optimization."
 (transform! expr
  (lambda (x) (and? (symbol? x) (bound? x)))
  (lambda (x) (eval x))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; list funs (unsorted):                                                      ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun any? (pred lst)
 "True when any lst members are pred."
 (if (nil? lst)
  nil
  (or?
   (pred (car lst))
   (any? pred (cdr lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun all? (pred lst)
 "True when all lst members are pred."
 (if (nil? lst)
  t
  (and?
   (pred (car lst))
   (all? pred (cdr lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun heads (lsts)
 (if (nil? lsts)
  nil
  (cons (car (car lsts)) (heads (cdr lsts)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun tails (lsts)
 (if (nil? lsts)
  nil
  (cons (cdr (car lsts)) (tails (cdr lsts)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun compose-preds preds
 "Does what it says on the tin and compose preds."
 (lambda (val)
  (let* ((fun
          (lambda (preds)
           (cond
            ((nil? (car preds))       t)
            ((nil? ((car preds) val)) nil)
            (t
             (fun (cdr preds)))))))
   (fun preds))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun intercalate (intercalated items)
 "Intercalate intercalated between items."
 (if (or? (nil? items) (nil? (cdr items)))
  items
  (cons (car items)
   (cons intercalated
    (intercalate intercalated (cdr items))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; log toggle helpers, these should be replaced with macros:                  ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun with-toggled-fun1 (toggled-fun)
 "Get a function that enables toggled-fun, evaluates fun-or-expr and sets toggled-fun back to it's prior state."
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun with-toggled-fun (toggled-fun)
 "Get a function that enables toggled-fun, evaluates fun-or-exprs and sets toggled-fun back to it's prior state."
 (lambda funs-or-exprs
  (last (mapcar (with-toggled-fun1 toggled-fun) funs-or-exprs))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! with-log-eval (with-toggled-fun log-eval))
(setq! with-log-core (with-toggled-fun log-core))
(setq! with-log-all  (with-toggled-fun log-all))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; random stuff that's all one section for now:                               ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun 1+      (n) (+ 1 n))                                                   ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun double  (n) (<< n 1))                                                  ;)
(setq! 2*      double)                                                        ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun benchmark (repetitions print-interval qexpr)
 "Benchmark expr by running it repetitions time and returning the total/average time in ms, printing updates ever print-interval iterations."
 (nl)
 (let ((ctr   0)
       (total 0))
  (repeat repetitions
   (setq! ctr (+ 1 ctr))
   (let ((before (time)))
    (eval qexpr)
    (setq! total (+ total (elapsed before))))
   (when (== 0 (% ctr print-interval))
    (nl)
    (princ "Iteration #")
    (princ ctr)
    (princ ", ")
    (princ (/ total 1000))
    (princ " ms so far.")))
  (nl)
  (princ "total ums: ")u
  (princ total) (nl)
  (princ "total ms: ")
  (princ (/ total 1000)) (nl)
  (princ "total s: ")
  (princ (/ total 1000000)) (nl)
  (let ((each-ms (/ total repetitions 1000)))
   (princ "each ms: ")
   (princ (/ total repetitions 1000))
   (nl)
   each-ms)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun max (a b)
 (if (> a b) a b))
(defun max (a b)
 (if (< a b) a b))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(defun list-depth (lst)
 "This one is untested."
 (if (atom? lst)
  0
  (max 1 (+ (list-depth (car lst)) (list-depth (cdr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)



