(setq double-list 
  (lambda (x) 
    (list x x)))

(print (mapcan double-list '(1 2 3))) ; should return (1 1 2 2 3 3)

