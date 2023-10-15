;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; 'standard library', such as it is:                                         ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; type predicates:                                                           ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! type?     (lambda (typ o) (eq?  typ     (type          o             ))))
(setq! atom?     (lambda (o)     (not (type?   :CONS          o             ))))
(setq! char?     (lambda (o)          (type?   :CHAR          o              )))
(setq! cons?     (lambda (o)          (type?   :CONS          o              )))
(setq! core?     (lambda (o)          (type?   :CORE          o              )))
(setq! env?      (lambda (o)          (type?   :ENV           o              )))
(setq! error?    (lambda (o)          (type?   :ERROR         o              )))
(setq! float?    (lambda (o)          (type?   :FLOAT         o              )))
(setq! integer?  (lambda (o)          (type?   :INTEGER       o              )))
(setq! lambda?   (lambda (o)          (type?   :LAMBDA        o              )))
(setq! macro?    (lambda (o)          (type?   :MACRO         o              )))
(setq! rational? (lambda (o)          (type?   :RATIONAL      o              )))
(setq! string?   (lambda (o)          (type?   :STRING        o              )))
(setq! symbol?   (lambda (o)          (type?   :SYMBOL        o              )))
(setq! improper? (lambda (o)     (and (tail? o) (not (proper? o            )))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; even?/odd? predicates:                                                     ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! even?     (lambda (n)               (and  (integer? n)  (== 0 (% n 2)))))
(setq! odd?      (lambda (n)               (and  (integer? n)  (== 1 (% n 2)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; equal? predicate:                                                          ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! equal?
 (lambda (o1 o2)
  (cond
   ((and (atom? o1) (atom? o2)) (eql? o1 o2))
   ((and (cons? o1) (cons? o2))
    (and (equal? (car o1) (car o2))
     (equal? (cdr o1) (cdr o2))))
   (t nil))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; compound car/cdrs:                                                         ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! cadr      (lambda (x)          (car (cdr x)))                           )
(setq! cdar      (lambda (x)          (cdr (car x)))                           )
(setq! cddr      (lambda (x)          (cdr (cdr x)))                           )
(setq! caar      (lambda (x)          (car (car x)))                           )
(setq! caaar     (lambda (x)     (car (car (car x))))                          )
(setq! caadr     (lambda (x)     (car (car (cdr x))))                          )
(setq! cadar     (lambda (x)     (car (cdr (car x))))                          )
(setq! caddr     (lambda (x)     (car (cdr (cdr x))))                          )
(setq! cdaar     (lambda (x)     (cdr (car (car x))))                          )
(setq! cdadr     (lambda (x)     (cdr (car (cdr x))))                          )
(setq! cddar     (lambda (x)     (cdr (cdr (car x))))                          )
(setq! cdddr     (lambda (x)     (cdr (cdr (cdr x))))                          )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; list funs:                                                                 ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! nth
 (lambda (n lst)
  (cond
   ((nil? lst)  nil)
   ((eql? n 0)  (car lst))
   (t           (nth (- n 1) (cdr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! last
 (lambda (lst)
  "Get last item in a list."
  (cond
   ((nil? lst) nil)
   ((nil? (cdr lst)) lst)
   (t (last (cdr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! last-old
 (lambda (lst)
  "Get last item in a list, old version."
  (if (or (nil? lst) (nil? (cdr lst)))
   lst
   (last (cdr lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; appenc/nconc!:                                                             ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! append
 (lambda (lst1 lst2)
  "Append two lists."
  (if (nil? lst1)
   lst2
   (cons (car lst1) (append (cdr lst1) lst2)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! nconc2!
 (lambda (lst1 lst2)
  "Destructively join two lists."
  (cond
   ((nil? lst1) lst2)
   (t (rplacd! (last lst1) lst2)
    lst1))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! nconc!
 (lambda lists
  "Destructively join many lists."
  (let ((result (car lists))
        (remaining (cdr lists)))
   (while (not (nil? remaining))
    (let ((tail (last result)))
     (rplacd! tail (car remaining))
     (setq! result tail))
    (setq! remaining (cdr remaining)))
   (car lists))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; list funs (push/push-back):                                                ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! push-back!
 (lambda (lst elem)
  "Destructively push elem onto the end of lst."
  (rplacd! (last lst) (cons elem nil))
  lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! push!
 (lambda (elem lst)
  "Destructively push elem onto the front of lst."
  (let ((old-car (car lst)))
   (rplaca! lst elem)
   (rplacd! lst (cons old-car (cdr lst)))
   lst)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! push-back
 (lambda (lst elem)
  "Non-destructively push elem onto the end of lst."
  (append lst (cons elem nil))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! push
 (lambda (elem lst)
  "Non-destructively push elem onto the front of lst, aka cons."
  (cons elem lst)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; list funs (reduce/rreduce/filter):                                         ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! reduce
 (lambda (fun acc lst)
  (if (nil? lst)
   acc
   (reduce fun (fun acc (car lst)) (cdr lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! rreduce
 (lambda (fun acc lst)
  (if (nil? lst)
   acc
   (fun (car lst) (rreduce fun acc (cdr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! filter
 (lambda (pred lst)
  (cond
   ((nil? lst) nil)
   ((pred (car lst))
    (cons (car lst) (filter pred (cdr lst))))
   (t (filter pred (cdr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; list funs (map variants):                                                  ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! mapcar
 (lambda (fun lst)
  (if (nil? lst)
   nil
   (cons (fun (car lst)) (mapcar fun (cdr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! mapc
 (lambda (fun lst)
  (if (nil? lst)
   nil
   (fun (car lst))
   (mapc fun (cdr lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! mapconcat
 (lambda (fun lst delimiter)
  (if (nil? lst)
   ""
   (reduce
    (lambda (acc item)
     (concat acc delimiter item))
    (fun (car lst))
    (mapcar fun (cdr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! mapcan
 (lambda (fun lst)
  (if (nil? lst)
   nil
   (let ((result (fun (car lst)))
         (rest   (mapcan fun (cdr lst))))
     (if (nil? result)
         rest
      (nconc! result rest))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; misc:                                                                      ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! 1+     (lambda (n) (+ 1 n)))                                           ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! double (lambda (n) (* 2 n)))                                           ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! transform!
 (lambda (obj pred fun)
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
   obj)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! transform
 (lambda (obj pred fun)
  (if (atom? obj)
   (if (pred obj)
    (fun obj)
    obj)
   (cons
    (transform (car obj) pred fun)
    (transform (cdr obj) pred fun)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! benchmark
 (lambda (repetitions print-interval qexpr)
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
    each-ms))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
;; log toggle helpers, these should be replaced with macros:                  ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! with-toggled-fun1
 (lambda (toggled-fun)
  (lambda (fun-or-expr)
   (if (lambda? fun-or-expr)
    (let* ((old    (toggled-fun t))
           (result (fun-or-expr))
           (new    (toggled-fun old)))
     result)
    (let* ((old    (toggled-fun t))
           (result (eval fun-or-expr))
           (new    (toggled-fun old)))
     result)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! with-toggled-fun
 (lambda (toggled-fun)
  (lambda funs-or-exprs
   (last (mapcar (with-toggled-fun1 toggled-fun) funs-or-exprs)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! with-log-eval (with-toggled-fun log-eval))
(setq! with-log-core (with-toggled-fun log-core))
(setq! with-log-all  (with-toggled-fun log-all))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; zip2:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! zip2
  (lambda (lst1 lst2)
    (if (or (nil? lst1) (nil? lst2))
        nil
        (cons (list (car lst1) (car lst2))
         (zip2 (cdr lst1) (cdr lst2))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; any:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! any?
  (lambda (pred lst)
    (if (nil? lst)
        nil
     (or (pred (car lst)) (any? pred (cdr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
