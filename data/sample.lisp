;(print (concat "hello" "world" "bang"))
(mapconcat (lambda (x) (concat x x)) '("hello" "world" "bang"))
