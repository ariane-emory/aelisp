o;;(write (is-unquote-expr? '(unquote 1))) (nl)

;;(write `(list 'a ,(+ 4 5))) (nl)

;; (write `(list 1 a ,x)) (nl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq! x 10)

(setq! lst '(1 2 3 4 5 6 7 8 9 10 11 12 13))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; complex version
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun transform (expr pred?)
  (cond
    ;; Handle atoms
    ((atom? expr)
     (if (pred? expr)
         (eval (cdr expr)) ; If the expression matches pred?, evaluate it
         (if (symbol? expr) ; Check if the expression is a symbol
             (list 'quote expr) ; Quote the symbol
             expr))) ; Return the expression unchanged
    ;; If the car matches the pred?
    ((pred? (car expr)) 
     (cons (eval (cadr (car expr))) ; Evaluate the unwrapped car
           (transform (cdr expr) pred?)))
    ;; Special handling for first item being 'quote
    ((eq? (car expr) 'quote)
     (list 'quote (cadr expr)))
    ;; Otherwise, recurse over car and cdr
    (t 
     (let ((new-car (transform (car expr) pred?))
           (new-cdr (transform (cdr expr) pred?)))
       (if (eq? new-cdr 'quote) ; Check if the new cdr is just a 'quote
           (cons new-car nil) ; If so, don't add it to the list
           (cons new-car new-cdr))))))

(defmacro unquote (expr) expr) 

(defmacro quasiquote (expr)
  (transform expr
   (lambda (x) (and
     (cons? x)
     (eq? (car x) 'unquote)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simple version
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun transform (obj pred? fun)
  (if (pred? obj)
   (fun obj)
   (if (atom? obj)
    obj
    (cons
     (transform (car obj) pred? fun)
     (transform (cdr obj) pred? fun)))))

(defun is-unquote-expr? (obj)
  (and (cons? obj) (eq? (car obj) 'unquote)))

(defmacro quasiquote (expr)
  (transform expr
   is-unquote-expr?
   second))

(defmacro unquote (expr) expr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (log-eval t)

(setq! lst-base '(1 2 3 4 5 6 ((7 8 (unquote 9) 10 11 12 13))))
(setq! lst lst-base)

(write (transform lst integer? 2*))
(nl)

(princ "Done.")
(nl)


(setq! lists (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

;; (log-macro t)
;; (log-macro nil)

;; (nl)
;; (write `,1)

;; (nl)
;; (eval `(mapcar princ ',lists))

;; (nl)
;; (write (zip '(a b c) '(1 2 3)))

;; ;; (nl)
;; ;; (old-zip '(a b c) '(1 2 3))

;; (nl)
;; (princ "Done.")

;; (log-core t)
;; (log-core nil)
;; (log-eval t)
;; (log-eval nil)

;; (princ "Found 5 in list at index ")
;; (write (indexql 5 lst))
;; (nl)
;; (princ "Found 88 in list at index ")
;; (write (indexql 88 lst))
;; (nl)
;; (princ "Found 5 in list at index ")
;; (write (indexq 5 lst))
;; (nl)
;; (princ "Found 88 in list at index ")
;; (write (indexq 88 lst))
;; (nl)

;; (princ "Removing 5 from list:  ") (write (removeq  5  lst)) (nl)
;; (princ "Removing 88 from list: ") (write (removeq  88 lst)) (nl)
;; (princ "Removing 5 from list:  ") (write (removeql 5  lst)) (nl)
;; (princ "Removing 88 from list: ") (write (removeql 88 lst)) (nl)

;; (log-eval t)
;; (memql? 5 lst)
;; (memql? 88 lst)
;; (log-eval nil)

;;(log-macro t)
;;(make-mem? memql? eql?)

;; (write (memq? 5 lst)) (nl)
;; (write (memql? 5 lst)) (nl)
;; (write (indexq 5 lst)) (nl)
;; (write (indexql 5 lst)) (nl)
;; (write (removeq 5 lst)) (nl)
;; (write (removeql 5 lst)) (nl)
