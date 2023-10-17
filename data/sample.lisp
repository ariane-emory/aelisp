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

;;(write (is-unquote-expr? '(unquote 1))) (nl)

;;(write `(list 'a ,(+ 4 5))) (nl)

;; (write `(list 1 a ,x)) (nl)

;; (log-eval t)

(setq! lst '(1 2 3 4 5 6 ((7 8 9) 10 11 12 13)))

(write (transform integer? 2*)) (nl)
