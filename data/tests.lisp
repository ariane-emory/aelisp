
(setq! *random-m* 4611686018427387904) ; 2^62
(setq! *random-a* 1664525)
(setq! *random-c* 1013904223)
(setq! *random-seed* (now-us))

(defun randomize (new-seed)
 "Set a new seed for the RNG."
 (setq! *random-seed* new-seed))

(defun abs (n)
 "Return the absolute value of N."
 (if (< n 0)
  (- n)
  n))

(defun random-inner ()
 "Return a random integer."
 (setq! *random-seed* (% (+ (mul *random-a* *random-seed*) *random-c*) *random-m*)))

(defun random-int-range rest
 ;; "Return a random integer between MIN (inclusive) and MAX (exclusive)."
 ;; (unless (or (nil? rest) (<= (length rest) 2))         (error "random-int-range takes either 0, 1 or 2 arguments"))
 ;; (unless (or (nil? (car rest)) (integer? (car rest)))  (error "if provided, first rest argument must be an integer"))
 ;; (unless (or (nil? (cdr rest)) (integer? (cadr rest))) (error "if provided, second rest argument must be an integer"))
 (let ((val  (setq! *random-seed* (% (+ (mul *random-a* *random-seed*) *random-c*) *random-m*))))
  *random-seed*))
 ;; (if (nil? rest)
 ;;  (random-inner)
 ;;  (let ((min (if (cadr rest) (car  rest) 0))
 ;;        (max (if (cadr rest) (cadr rest) (car rest))))
 ;;   (let ((range (- max min)))
 ;;    (+ min (mod (abs (random-inner)) range)))))))

(repeat 10 (princ (random-int-range))         (nl))
(repeat 10 (princ (random-int-range 10))      (nl))
(repeat 10 (princ (random-int-range -10 10))  (nl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(nl)
(princ "All tests passed.")
(provide 'tests)
(nl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

