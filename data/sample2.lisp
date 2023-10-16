;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; construction zone
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(princ "Odd numbers: ") (write (filter (compose-preds integer? odd?) '(1 2 3 4 5 6 7 8 9 10))) (nl)
(write (any? (compose-preds integer? even?) '(1 3 a 5))) (nl)
(write (any? (compose-preds integer? even?) '(1 3 a 4 5))) (nl)

(write ((compose-preds integer? odd?)  7)) (nl)
(write ((compose-preds integer? odd?)  8)) (nl)
(write ((compose-preds integer? odd?) 'a)) (nl)

(writen '(heads '((1 2 3) (4 5 6) (7 8 9))))
(writen '(tails '((1 2 3) (4 5 6) (7 8 9))))

(writen       '(all? nil? '(nil nil nil)))        
(writen       '(all? nil? '(nil nil 1  )))        
(writen '(not? (all? nil? '(nil nil nil)))) 
(writen '(not? (all? nil? '(nil nil 1  )))) 

(princn  "this" "is" "a" "test")
(princns "this" "is" "a" "test")

(princ "princns: ") (princns 1 2 3 4 5 6 7 8 9)

(princn (or?  nil nil 3 8))
(princn (and? 3 8 nil))
(princn (and? 3 7 8))

(princni ", " 1 2 3)

(setq! test
 (lambda args
  (princ "car-ed head " (car args)) (nl)
  (princ "cdr-ed tail " (cdr args)) (nl)))

(test 1 2 3)

(setq! test
 (lambda (head . tail)
  tail))

(setq! test
 (lambda (head head2 . tail)
  (princ "head "  head) (nl)
  (princ "head2 " head2) (nl)
  (princ "tail "  tail) (nl)))

(test 1 2 3 4 5)

(princn "got here")
(princn ''(1 . 2))

(princn ''(1 2))

(setq! test
 (lambda args
  (cdr args)))

(princn '(test 1 2 3 4 5))

(setq! test
 (macro (foo bar)
  (list quote (list foo bar))))

(princ "test: ") (write (test 12 34)) (nl)

(princ "Reached the end.") (nl)






