;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (vector-style list API):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun list-set! (lst index obj)
 "Destructively set the element at INDEX of LST to OBJ."
 (unless (list? lst) (error "LST must be a list"))
 (unless (and (integer? index) (>= index 0)) (error "INDEX must be a non-negative integer"))
 (let ((current-index 0)
       (done nil))
  (while (and lst (not done))
   (if (= current-index index)
    (progn
     (rplaca! lst obj)
     (setq! done t))
    (setq! lst (cdr lst))
    (setq! current-index (+ 1 current-index))))
  (unless done (error "INDEX out of bounds")))
 obj)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun list-ref (lst index)
 "Return the element at INDEX of LST."
 (unless (list? lst) (error "LST must be a list"))
 (unless (and (integer? index) (>= index 0)) (error "INDEX must be a non-negative integer"))
 (let ((current-index 0))
  (while (and lst (not (= current-index index)))
   (setq! lst (cdr lst))
   (setq! current-index (+ 1 current-index)))
  (if lst
   (car lst)
   (error "INDEX out of bounds"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! list-length length)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-list (size init-val)
 "Make a new list of length SIZE with its cars set to INIT-VAL."
 (unless (integer? size) (error "SIZE must be an integer"))
 (let ((result nil)
       (current-index 0))
  (while (< current-index size)
   (setq! result (cons init-val result))
   (setq! current-index (+ 1 current-index)))
  result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'vector-list)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
