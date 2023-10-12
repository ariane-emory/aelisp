

(setq reduce
 (lambda (fun acc lst)
  (if (nil? lst)
      acc
   (reduce fun (fun acc (car lst)) (cdr lst)))))


(setq rreduce
  (lambda (fun acc lst)
    (if (nil? lst)
      acc
      (fun (car lst) (rreduce fun acc (cdr lst))))))

(reduce + 0 '(1 2 3 4 5)) ; => 15
(rreduce - 0 '(1 2 3)) 

(concat "hello" "world")
(concat "hello" 1)
