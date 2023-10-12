; (mapconcat (lambda (x) (concat x x)) '("hello" "world" "bang"))

(setq filter
 (lambda (pred lst)
  (cond
    ((nil? lst) nil)                               ; If the list is empty, return nil
    ((pred (car lst))                              ; If the predicate is true for the current element
     (cons (car lst) (filter pred (cdr lst))))     ; Keep the element and continue with the rest of the list
    (t (filter pred (cdr lst))))))                 ; Otherwise, continue with the rest of the list without the current element

 (setq is-odd
  (lambda (n) 
    (not (== 0 (% n 2)))))

(filter is-odd '(1 2 3 4 5 6 7 8 9 10))   ; Expected result: (1 3 5 7 9)

(write (filter (lambda (x) (not (nil? x))) '(a nil b c nil d)))   ; Expected result: (a b c d)
