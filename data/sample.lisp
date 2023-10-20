(display (union2 memql? '(1 2 3 4) '(4 5 2 2)))
(nl)

(display (unionq'(1 2 3 4) '(4 5 2 2) '(5 6 7 8)))
(nl)

(write (letrec
 ((factorial
   (lambda (n)
    (if (< n 2)
     1
     (* n (factorial (- n 1)))))))
 (factorial 5)))
(nl)

(defun mmemql? (x lst)
 (letrec
  ((chase
    (lambda (x lst)
     (cond
      ((eql? x (car lst)) t)
      (lst (mmemql? x (cdr lst)))))))
  (chase x lst)))

(write (mmemql? 4 '(1 2 3 4 5 6 7)))
(nl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(exit)

(defun curry1 (fun arg1)
 (lambda args
  (apply fun arg1 args)))

(setq! 2+ (curry1 + 2))

(log-eval t)
(write (2+ 3)) (nl)

