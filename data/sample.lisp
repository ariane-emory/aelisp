(write (heads '((1 2 3) (4 5 6) (7 8 9)))) (nl)
(write (tails '((1 2 3) (4 5 6) (7 8 9)))) (nl)

(write (all? nil? '(nil nil nil)))         (nl)
(write (all? nil? '(nil nil 1  )))         (nl)
(write (not? (all? nil? '(nil nil nil))))  (nl)
(write (not? (all? nil? '(nil nil 1  ))))  (nl)

(princn "this" "is" "a" "test")

(setq! intercalate
 (lambda (intercalated lst)
  (if (or? (nil? lst) (nil? (cdr lst))) ; Check if the list is empty or has only one element.
   lst
   (cons (car lst) 
    (cons intercalated
     (intercalate intercalated (cdr lst)))))))

(write (intercalate " " '(1 2 3 4 5 6 7 8 9))) (nl)
