


(write (heads '((1 2 3) (4 5 6) (7 8 9)))) (nl)
(write (tails '((1 2 3) (4 5 6) (7 8 9)))) (nl)





(write (all? nil? '(nil nil nil)))       (nl)
(write (all? nil? '(nil nil 1  )))       (nl)
(write (not? (all? nil? '(nil nil nil)))) (nl)
(write (not? (all? nil? '(nil nil 1  )))) (nl)

