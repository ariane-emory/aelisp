;; old version
(setq transform!
      (lambda (pred fun obj)
        (if (not (eq :CONS (type obj))) 
            (if (pred obj) 
                (setf obj (fun obj)))
            (let ((head (car obj))
                  (tail (cdr obj)))
              (cond 
                ((nil? obj) obj)
                ((pred head)
                 (rplaca obj (fun head)))
                ((eq :CONS (type head))
                 (transform! pred fun head)))
              (if tail
                  (rplacd obj (transform! pred fun tail)))
              (if (and (not (eq :CONS (type tail))) (pred tail))
                  (rplacd obj (fun tail)))))
        obj))

;; verbose version
(setq transform!
      (lambda (pred fun obj)
        (let ((process-cons
                (lambda (obj)
                  (let ((head (car obj))
                        (tail (cdr obj)))
                    (when (pred head)
                      (rplaca obj (fun head)))
                    (when (eq :CONS (type head))
                      (transform! pred fun head))
                    (when tail
                      (rplacd obj (transform! pred fun tail)))
                    (when (and (not (eq :CONS (type tail))) (pred tail))
                      (rplacd obj (fun tail)))))))
          (cond 
            ((and (not (eq :CONS (type obj))) (pred obj))
             (setf obj (fun obj)))
            ((eq :CONS (type obj))
             (process-cons obj)))
          obj)))

;; short version
(setq transform!
      (lambda (pred fun obj)
        (cond
          ((pred obj) (setf obj (fun obj)))
          ((eq :CONS (type obj))
           (let ((head (car obj))
                 (tail (cdr obj)))
             (cond
               ((pred head) (rplaca obj (fun head)))
               ((eq :CONS (type head)) (transform! pred fun head)))         
             (if tail (rplacd obj (transform! pred fun tail)))
             (if (and (not (eq :CONS (type tail))) (pred tail))
                 (rplacd obj (fun tail)))))
          (t obj))
        obj)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq double   (lambda (x) (* 2 x)))
(setq integer? (lambda (x) (eq :INTEGER (type x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq transform!
      (lambda (pred fun obj)
        (if (not (eq :CONS (type obj)))
            (error "obj must be a list")
            (cond
              ((pred obj) (setf obj (fun obj)))
              ((eq :CONS (type obj))
               (let ((head (car obj))
                     (tail (cdr obj)))
                 (cond
                   ((pred head) (rplaca obj (fun head)))
                   ((eq :CONS (type head))  (transform! pred fun head)))
                 (cond
                   ((pred tail) (rplacd obj (fun tail)))
                   ((eq :CONS (type tail))  (rplacd obj (transform! pred fun tail))))))
              (t obj))
            obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq l '(2 (4 8)))
(transform! integer? double l)
(print l) ;; sucessfully prints (4 (8 16)).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq l (transform! integer? double '(2 (4 8))))
(print l) ;; successfully prints (4 (8 16)).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq l '(2 (4 . a)))
(transform! (lambda (obj) (eq :INTEGER (type obj))) (lambda (num) (* 2 num)) l)
(print l) ;; successdully prints (4 (8 . a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq l (transform! integer? double '(2 (4 . 8))))
(print l) ;; successfully prints (4 (8 . 16))!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq l 2)
(transform! integer? double l)
(print l) ;; should return an :ERROR

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print (error "fuck"))
(print (error "shit" '((thing . "boop"))))
