;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*-lisp-indent-function: 1; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq double   (lambda (x) (* 2 x)))
(setq integer? (lambda (x) (eq :INTEGER (type x))))
(setq cons?    (lambda (x) (eq :CONS    (type x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mcm; time { for i in {1..10000}; do ./bin/ae; done; }
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when nil
  (let* ((𝑛 30)
       (∨ ∨)
       (*memo* '((2 . 1) (1 . 1)))
       (memoize (lambda (k v) (cdr (car (≔    *memo* (aset *memo* k v))))))
       (𝑓       (lambda (𝑥)
                  (let  ((memoized (aget *memo*  𝑥)))
                    (∨    memoized
                          (memoize  𝑥 (+ (𝑓 (- 𝑥 1))
                                         (𝑓 (- 𝑥 2)))))))))
  (𝑓 𝑛)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq double   (lambda (x) (* 2 x)))
(setq integer? (lambda (x) (eq :INTEGER (type x))))
(setq cons?    (lambda (x) (eq :CONS    (type x))))

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

(setq transform!
  (lambda (pred fun obj)
    (if (not (eq :CONS (type obj)))
        (error "obj must be a list")
        (let ((head (car obj))
              (tail (cdr obj)))
          ;; Check and potentially transform the head
          (cond
            ((pred head) (rplaca obj (fun head)))
            ((eq :CONS (type head)) (transform! pred fun head)))

          ;; Check and potentially transform the tail
          (cond
            ((pred tail) 
             (rplacd obj (fun tail))
             ;; Handle the rest of the list
             (if (eq :CONS (type (cdr obj)))
                 (transform! pred fun (cdr obj))))
            ((eq :CONS (type tail))
             (transform! pred fun tail)))))
    obj))

(setq transform!
  (lambda (pred fun obj)
    (if (not (eq :CONS (type obj)))
        (error "obj must be a list")
        (let ((head (car obj))
              (tail (cdr obj)))
          ;; Check and potentially transform the head
          (cond
            ((pred head) (rplaca obj (fun head)))
            ((eq :CONS (type head)) (transform! pred fun head)))

          ;; Check and potentially transform the tail
          (cond
            ((pred tail) 
             ;; Instead of replacing the entire tail, 
             ;; just update the current cons cell to point to the transformed value
             (setf (cdr obj) (fun tail)))
            ((eq :CONS (type tail))
             (transform! pred fun tail)))))
    obj))


(setq transform!
  (lambda (pred fun obj)
    (if (not (eq :CONS (type obj)))
        (error "obj must be a list")
        (let ((head (car obj))
              (tail (cdr obj)))
          ;; Check and potentially transform the head
          (cond
            ((pred head) (rplaca obj (fun head)))
            ((eq :CONS (type head)) (transform! pred fun head)))

          ;; Check and potentially transform the tail
          (cond
            ((pred tail) 
             ;; Update the cdr of the current cons cell to point to the transformed value
             (rplacd obj (fun tail)))
            ((eq :CONS (type tail))
             (transform! pred fun tail)))))
    obj))

(setq transform!
  (lambda (pred fun obj)
    (if (not (eq :CONS (type obj)))
        (error "obj must be a list")
        (let ((head (car obj))
              (tail (cdr obj)))
          ;; Check and potentially transform the head
          (cond
            ((pred head) (rplaca obj (fun head)))
            ((eq :CONS (type head)) (transform! pred fun head)))

          ;; Check and potentially transform the tail
          (cond
            ((pred tail)
             (if (eq :CONS (type obj))
                 (rplacd obj (fun tail))
                 (setf obj (fun tail))))
            ((eq :CONS (type tail))
             (transform! pred fun tail)))))
    obj))

(setq transform!
  (lambda (pred fun obj)
    (if (not (eq :CONS (type obj)))
        (error "obj must be a list")
      (let ((head (car obj))
            (tail (cdr obj)))
        ;; Check and potentially transform the head
        (cond
          ((pred head) (rplaca obj (fun head)))
          ((eq :CONS (type head)) (transform! pred fun head)))

        ;; Check and potentially transform the tail
        (cond
          ((pred tail) 
           ;; Don't directly update obj, just rplacd or rplaca
           (if (eq :CONS (type obj))
               (rplacd obj (fun tail))))
          ((eq :CONS (type tail))
           (transform! pred fun tail)))))
    obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq l '(2 (4 8)))
(transform! integer? double l)
(print l) ;; case 1: sucessfully prints (4 (8 16)).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq l (transform! integer? double '(2 (4 8))))
(print l) ;; case 2: successfully prints (4 (8 16)).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq l '(2 (4 . a)))
(transform! (lambda (obj) (eq :INTEGER (type obj))) (lambda (num) (* 2 num)) l)
(print l) ;; case 3: successdully prints (4 (8 . a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq l (transform! integer? double '(2 (4 . 8))))
(print l) ;; case 5: successfully prints (4 (8 . 16))!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq l 2)
(transform! integer? double l)
(print l) ;; correctly returns an :ERROR

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print (transform!
        (lambda (x) (and (proper? x) (eql (length (car x)) 2)))
        (lambda (x) :REPLACED)
        '(1 (2 3) (4 5 6)))) ;; case 6: prints (1 :REPLACED), but i want (1 :REPLACED (4 5 6)). Not sure if problem is with transform! or its arguments?

(4 (8 16))
(4 (8 16))
(4 (8 . a))
(4 (8 . 16))
CORE<error> returned an error: ERROR<obj must be a list>
CORE<if*> returned an error: ERROR<obj must be a list>
2
(1 . :REPLACED)
