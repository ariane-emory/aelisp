(setq! lst-base '(1 a 2 b 3 4 5 6 ((7 8 (unquote 9) 10 11 12 13))))
(setq! lst-base '(1 a 2 b 3 ((4 (unquote 5) 6))))
(setq! lst-base '(1 a 2))
(setq! lst-base '(1 a (unquote 2)))
(setq! lst lst-base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro quotify (x) $('quote x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; No recursion:

(defmacro xform (obj)
 (cond
  ((and (cons? obj) (cdr obj)) (list 'cons (list 'quote (car obj)) (list 'xform (cdr obj))))
  ((cons? obj)                 (list 'cons (list 'quote (car obj)) nil))
  (t                           obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; With recursion:

(defmacro xform (obj)
  (cond
   ((nil? obj) nil)
   ((cons? obj)
    (let ((head (car obj))
          (tail (cdr obj)))
      (list 'cons
            (if (cons? head) 
                (list 'xform head)
                (list 'quote head))
            (list 'xform tail))))
   (t obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro xform (obj)
  (cond
   ;; Handle end of list
   ((nil? obj) nil)

   ;; Handle (unquote something)
   ((and (cons? obj) (eq? 'unquote (car obj)))
    (cadr obj))

   ;; Handle list item
   ((cons? obj) 
    (list 'cons 
          (if (and (cons? (car obj)) (eq? 'unquote (car (car obj))))
              (cadr (car obj))
              (list 'quote (car obj)))
          (xform (cdr obj))))

   ;; Handle single items
   (t obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro xform (obj)
  (cond
   ;; Handle end of list
   ((nil? obj) nil)

   ;; Handle (unquote something)
   ((and (cons? obj) (eq? 'unquote (car obj)))
    (cadr obj))

   ;; Handle list item where the tail is another cons cell
   ((and (cons? obj) (cons? (cdr obj))) 
    (list 'cons 
          (if (and (cons? (car obj)) (eq? 'unquote (car (car obj))))
              (cadr (car obj))
              (xform (car obj)))
          (xform (cdr obj))))

   ;; Handle list item where the tail is not another cons cell (improper list)
   ((cons? obj)
    (list 'cons
          (if (and (cons? (car obj)) (eq? 'unquote (car (car obj))))
              (cadr (car obj))
              (xform (car obj)))
          (list 'quote (cdr obj))))

   ;; Handle single items
   (t (list 'quote obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro xform (obj)
  (cond
   ;; Handle end of list
   ((nil? obj) nil)

   ;; Handle (unquote something)
   ((and (cons? obj) (eq? 'unquote (car obj)))
    (cadr obj))

   ;; Handle cons cells
   ((cons? obj)
    (list 'cons 
          (list 'xform (car obj))
          (list 'xform (cdr obj))))

   ;; Handle everything else
   (t (list 'quote obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro xform (obj)
  (cond
   ;; Handle end of list
   ((nil? obj) nil)

   ;; Handle (unquote something)
   ((and (cons? obj) (eq? 'unquote (car obj)))
    (cadr obj))

   ;; Handle cons cells
   ((cons? obj)
    (let ((head (car obj))
          (tail (cdr obj)))
      (list 'cons 
            (if (cons? head) 
                (list 'xform head)
                (list 'quote head))
            (list 'xform tail))))

   ;; Handle everything else
   (t (list 'quote obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(log-macro t)

(nl) (princ "Before.")
(xform (list 1 2 (list 3 4)))
(nl) (princ "After.")
(nl)

(nl) (princ "Before.")
(xform 1)
(nl) (princ "After.")
(nl)

(nl) (princ "Before.")
(xform nil)
(nl) (princ "After.")
(nl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nl)
(princ "Done.")
(exit)shove
