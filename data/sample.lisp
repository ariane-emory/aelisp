(setq x 8)

(cond
 ((eql? 8 x) 111 222)   
 (t 333))



(setq nth
 (lambda (n lst)
  (cond
    ((nil? lst)   nil) 
    ((eql? n 0)   (car lst))
    (t            (nth (- n 1) (cdr lst))))))

(nth 3 '(1 2 3 4 5))

