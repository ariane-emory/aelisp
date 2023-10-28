;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; benchmark:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun benchmark (repetitions print-interval qexpr)
 "Benchmark QEXPR by running it REPETITIONS times and returning the"
 "total/average time in ms, printing updates ever PRINT-INTERVAL iterations."

 "THIS PROBABLY NEEDS AN UPDATE!"
 (unless (integer? repetitions)   (error "REPETITIONS must be an integer"))
 (unless (integer? print-interval)(error "PRINT-INTERVAL must be an integer"))
 (nl)
 (let ((ctr   0)
       (total 0))
  (repeat repetitions
   (setq! ctr (1+ ctr))
   (let ((before (time)))
    (eval qexpr)
    (setq! total (+ total (elapsed before))))
   (when (zero? (% ctr print-interval))
    (nl)
    (princ "Iteration #")
    (princ ctr)
    (princ ", ")
    (princ (÷ total 1000))
    (princ " ms so far.")))
  (nl)
  (princ "total ums: ")
  (princ total) (nl)
  (princ "total ms: ")
  (princ (÷ total 1000)) (nl)
  (princ "total s: ")
  (princ (÷ total 1000000)) (nl)
  (let ((each-ms (÷ total repetitions 1000)))
   (princ "each ms: ")
   (princ (÷ total repetitions 1000))
   (nl)
   each-ms)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'benchmark)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
