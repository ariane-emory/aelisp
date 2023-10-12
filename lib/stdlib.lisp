;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 'standard library', such as it is:                        ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! improper? (lambda (o)   (and (tail? o) (not (proper? o               )))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! type?     (lambda (t o)      (eq?      t     (type   o                ))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! atom?     (lambda (o)   (not (type?   :CONS          o                ))))
(setq! char?     (lambda (o)        (type?   :CHAR          o                 )))
(setq! cons?     (lambda (o)        (type?   :CONS          o                 )))
(setq! core?     (lambda (o)        (type?   :CORE          o                 )))
(setq! env?      (lambda (o)        (type?   :ENV           o                 )))
(setq! error?    (lambda (o)        (type?   :ERROR         o                 )))
(setq! float?    (lambda (o)        (type?   :FLOAT         o                 )))
(setq! integer?  (lambda (o)        (type?   :INTEGER       o                 )))
(setq! lambda?   (lambda (o)        (type?   :LAMBDA        o                 )))
(setq! macro?    (lambda (o)        (type?   :MACRO         o                 )))
(setq! rational? (lambda (o)        (type?   :RATIONAL      o                 )))
(setq! string?   (lambda (o)        (type?   :STRING        o                 )))
(setq! symbol?   (lambda (o)        (type?   :SYMBOL        o                 )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! cadr      (lambda (x)        (car (cdr x)))                              )
(setq! cdar      (lambda (x)        (cdr (car x)))                              )
(setq! cddr      (lambda (x)        (cdr (cdr x)))                              )
(setq! caar      (lambda (x)        (car (car x)))                              )
(setq! caaar     (lambda (x)   (car (car (car x))))                             )
(setq! caadr     (lambda (x)   (car (car (cdr x))))                             )
(setq! cadar     (lambda (x)   (car (cdr (car x))))                             )
(setq! caddr     (lambda (x)   (car (cdr (cdr x))))                             )
(setq! cdaar     (lambda (x)   (cdr (car (car x))))                             )
(setq! cdadr     (lambda (x)   (cdr (car (cdr x))))                             )
(setq! cddar     (lambda (x)   (cdr (cdr (car x))))                             )
(setq! cdddr     (lambda (x)   (cdr (cdr (cdr x))))                             )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! 1+        (lambda (x)        (+ 1 x)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! nth
 (lambda (n lst)
  (cond
   ((nil? lst)  nil) 
   ((eql? n 0)  (car lst))
   (t           (nth (- n 1) (cdr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! transform!
 (lambda (expr pred fun)
  (if (not (eq :CONS (type expr)))
   (error "expr must be a list")
   (cond
    ((pred expr) (setf expr (fun expr)))
    ((eq :CONS (type expr))
     (let ((head (car expr))
           (tail (cdr expr)))
      (cond
       ((pred head) (rplaca! expr (fun head)))
       ((eq :CONS (type head))  (transform! head pred fun)))
      (cond
       ((pred tail) (rplacd! expr (fun tail)))
       ((eq :CONS (type tail))  (rplacd! expr (transform! tail pred fun))))))
    (t expr))
   expr)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! benchmark
 (lambda (repetitions print-interval qexpr)
  (nl)
  (let ((ctr 0 )
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
   (princ "total ums: ")
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! reduce
 (lambda (fun acc lst)
  (if (nil? lst)
   acc
   (reduce fun (fun acc (car lst)) (cdr lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! rreduce
 (lambda (fun acc lst)
  (if (nil? lst)
   acc
   (fun (car lst) (rreduce fun acc (cdr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
(setq! mapcar
 (lambda (fun lst)
  (if (nil? lst)
   nil
   (cons (fun (car lst)) (mapcar fun (cdr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! mapc
 (lambda (fun lst)
  (if (nil? lst)
   nil
   (progn
    (fun (car lst))
    (mapc fun (cdr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (setq! mapconcat
  (lambda (fun lst delimiter)
   (if (nil? lst)
    ""
    (reduce 
     (lambda (acc item) 
      (concat acc delimiter item)) 
     (fun (car lst)) 
     (mapcar fun (cdr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! mapcan!
  (lambda (fun lst)
    (if (nil? lst)
        nil
        (nconc (fun (car lst)) (mapcan fun (cdr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! filter
 (lambda (pred lst)
  (cond
    ((nil? lst) nil)
    ((pred (car lst))
     (cons (car lst) (filter pred (cdr lst))))
    (t (filter pred (cdr lst))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! append
  (lambda (lst1 lst2)
    (if (nil? lst1)
        lst2
        (cons (car lst1) (append (cdr lst1) lst2)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! nconc!
 (let ((x 1))
  (lambda (lst1 lst2)
    (cond
      ((nil? lst1) lst2)
      (t (rplacd! (last lst1) lst2) lst1)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! last
  (lambda (lst)
    (if (or (nil? lst) (nil? (cdr lst)))
        lst
        (last (cdr lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! push-back!
 (lambda (lst elem)
  (rplacd! (last lst) (cons elem nil))
  lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! push!
 (lambda (elem lst)
   (let ((new-cons (cons elem nil)))
     (rplacd! new-cons lst)
     new-cons)))
