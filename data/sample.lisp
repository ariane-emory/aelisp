(setq double-list 
  (lambda (x) 
    (list x x)))

(print (mapcan double-list '(1 2 3))) ; should return (1 1 2 2 3 3)
(print (last '(1 2 3))) ; should return (1 1 2 2 3 3)
; For mapc - prints elements of the list
(print (mapc (lambda (x) (princ x) (nl)) '(1 2 3 4)))
; Expected Output: 1 2 3 4 (each on a new line)

; For mapconcat - joins elements of the list with "-"
;(print (mapconcat (lambda (x) (eval (list 'int-to-string x))) '(1 2 3 4) "-"))
; Expected Output: "1-2-3-4"

; For mapcan - flattens the nested lists
(print (mapcan (lambda (x) (list x x)) '(1 2 3 4)))
; Expected Output: (1 1 2 2 3 3 4 4)
(exit)
