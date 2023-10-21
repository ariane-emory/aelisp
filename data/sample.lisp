
;; (setq! lst (union2 memql? '(1 2 3 4) '(4 5 2 2)))
(setq! lst '(1 2 3 4 5 6 7 8 9 10))

(princ "initial lst:     ") (write lst)                  (nl)
;; (log-eval t) (log-core t)
(princ "memql? 2:        ") (write (memql?   3 lst))     (nl)
(princ "removeql 4:      ") (write (removeql 4 lst))     (nl)
(princ "indexql 5:       ") (write (indexql  5 lst))     (nl)
;; (log-core t)
(princ "mapcar!:         ") (write (mapcar! double lst)) (nl)
(princ "doubled:         ") (write lst)                  (nl)
;; (log-eval t)
;; (log-core t)
(princ "butlast:         ") (write (butlast '(1 2 3 4 5)))             (nl)
;; (exit)
(princ "reverse:         ") (write (reverse '(1 2 3 4 5)))             (nl)
(princ "reverse butlast: ") (write (reverse (butlast '(1 2 3 4 5))))   (nl)
(princ "union:           ") (write (unionql '(1 2 3) '(4 5 6)))        (nl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq! lst (make-list 6 200))
(princ "make-list:       ") (write lst) (nl)

(list-set! lst 0 100)
(princ "list-set! 0 100: ") (write lst) (nl)
(princ "list-ref  0:     ") (write (list-ref lst 0)) (nl)

(list-set! lst 1 101)
(princ "list-set! 1 101: ") (write lst) (nl)
(princ "list-ref  1:     ") (write (list-ref lst 1)) (nl)

(list-set! lst 2 102)
(princ "list-set! 2 102: ") (write lst) (nl)
(princ "list-ref  2:     ") (write (list-ref lst 2)) (nl)

(list-set! lst 3 103)
(princ "list-set! 3 103: ") (write lst) (nl)
(princ "list-ref  3:     ") (write (list-ref lst 3)) (nl)

(list-set! lst 4 104)
(princ "list-set! 4 104: ") (write lst) (nl)
(princ "list-ref  4:     ") (write (list-ref lst 4)) (nl)

(list-set! lst 5 105)
(princ "list-set! 5 105: ") (write lst) (nl)
(princ "list-ref  5:     ") (write (list-ref lst 5)) (nl)

;; (exit)

;;(write (list-ref lst 4))

(nl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun curry1 (fun arg1)
;;  (lambda args
;;   (apply fun arg1 args)))

;; (setq! 2+ (curry1 + 2))

;; (log-eval t)
;; (write (2+ 3))
;; (nl)

;; (defmacro test-it (args . cond-clauses)
;;  (write (car args)) (nl)
;;  (write (cadr args)) (nl)
;;  (write (eq? 'lst (cadr args))) (nl)
;;  (cond
;;   ((not (or (eq? 'lst (first args)) (eq? 'lst (second args))))
;;    (error "one of the args must be the symbol 'lst"))
;;   (t (error "found lst"))))

;; (test-it (obj j))

(defun lshift4 (n)
 (<< n 4))

(defun add-logging-to (fun)
 "Add logging to a function."
 (let* ((fun-name      (get (props fun) :last-bound-to))
        (old-fun-body  (body fun))
        (old-body-tail (cdr old-fun-body))
        (new-body
         `((princ
            "Applying "
            ',fun-name
            " to parameters "
            (syms (env))
            " with arguments "
            (vals (env))
            ".")
           (nl)
           (let ((result (progn ,@old-body-tail)))
            (princ "Result of applying "
             ',fun-name
             " was "
             result
             ".")
            (nl)
            result))))
  (rplacd! (body fun) new-body))
 fun)

(add-logging-to lshift4) (nl)
(princ 'lshift4 "'s body is now " (body lshift4)) (nl) (nl)
(princ "Call returned " (lshift4 4) ".") (nl)
