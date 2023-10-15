;; (writen '(heads '((1 2 3) (4 5 6) (7 8 9))))
;; (writen '(tails '((1 2 3) (4 5 6) (7 8 9))))

;; (writen       '(all? nil? '(nil nil nil)))        
;; (writen       '(all? nil? '(nil nil 1  )))        
;; (writen '(not? (all? nil? '(nil nil nil)))) 
;; (writen '(not? (all? nil? '(nil nil 1  )))) 

;; (princn  "this" "is" "a" "test")
;; (princns "this" "is" "a" "test")

;; (princns 1 2 3 4 5 6 7 8 9)

;; (princn (or?  nil nil 3 8))
;; (princn (and? 3 8 nil))
;; (princn (and? 3 7 8))


;; (princni ", " 1 2 3)

;; (setq! test
;;  (lambda args
;;   (nl) (princ "head " (car args))
;;   (nl) (princ "tail " (cdr args))))

;; (test 1 2 3)

;; (princn '(1 . 2))

(setq! test
 (lambda (head . tail)
  tail))

(test 1 2 3)

(exit)

(setq! test
 (lambda (head . tail)
  (nl) (princ "head " head)
  (nl) (princ "tail " tail)))

(log-eval t)

(test 1 2 3)

