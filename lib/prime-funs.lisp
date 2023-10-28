;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; primes:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prime? (num)
 "Check if a number is prime."
 (unless (integer? num) (error "NUM must be an integer"))
 (if (or (= num 0) (= num 1))
  nil
  (let ((limit (/ num 2))
        (divisor 2)
        (is-prime t))
   (while (and (<= divisor limit) is-prime)
    (if (= (% num divisor) 0)
     (setq! is-prime nil)
     (setq! divisor (+ divisor 1))))
   is-prime)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun primes (n)
 "Return the first n prime numbers."
 (unless (integer? n) (error "N must be an integer"))
 (let ((count 0)
       (num 2)
       (primes '()))
  (while (< count n)
   (when (prime? num)
    (setq! count (+ count 1))
    (setq! primes (append2 primes (list num))))
   (setq! num (+ num 1)))
  primes))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'prime-funs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
