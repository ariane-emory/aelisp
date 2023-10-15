(write (heads '((1 2 3) (4 5 6) (7 8 9)))) (nl)
(write (tails '((1 2 3) (4 5 6) (7 8 9)))) (nl)

(write (all? nil? '(nil nil nil)))         (nl)
(write (all? nil? '(nil nil 1  )))         (nl)
(write (not? (all? nil? '(nil nil nil))))  (nl)
(write (not? (all? nil? '(nil nil 1  ))))  (nl)

(princn "this" "is" "a" "test")

(setq! intercalate
 (lambda (intercalated items)
  (if (or? (nil? items) (nil? (cdr items))) ; Check if the list is empty or has only one element.
   items
   (cons (car items) 
    (cons intercalated
     (intercalate intercalated (cdr items)))))))

(write (intercalate " " '(1 2 3 4 5 6 7 8 9))) (nl)
