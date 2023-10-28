(require 'map)
(require 'misc-predicates)
(require 'quasiquote)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (unsorted):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun list* args
 (let*
  ((chase
	  (lambda (args)
		 (cond
      ((nil? args)       nil)
		  ((nil? (cdr args)) (car args))
		  (t                 (cons (car args) (chase (cdr args))))))))
  (chase args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun depth (lst)
 "Get the depth of a nested list structure."
 (unless (list? lst) (error "LST must be a list"))
 (let ((stack (list (cons lst 1))) ; Stack with initial list and depth of 1
       (max-depth 0))
  (while stack
   (let* ((current (pop! stack))
          (current-list (car current))
          (current-depth (cdr current)))
    (if (> current-depth max-depth)
     (setq! max-depth current-depth))
    (mapc (lambda (item)
           (when (list? item)
            (push! (cons item (1+ current-depth)) stack)))
     current-list)))
  max-depth))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun filter (pred? lst)
 "Return a list containing those members of lst satisfying pred?."
 (unless (fun? pred?) (error "PRED? must be a function"))
 (unless (list? lst)  (error "LST must be a list"))
 (let ((result nil))
  (while lst
   (if (pred? (car lst))
    (setq! result (cons (car lst) result)))
   (setq! lst (cdr lst)))
  (reverse result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun intercalate (intercalated lst)
 "Intercalate INTERCALATED between items in LST."
 (unless (list? lst) (error "LST must be a list"))
 (let ((result nil))
  (while (cdr lst)
   (setq! result (cons (car lst) result))
   (setq! result (cons intercalated result))
   (setq! lst (cdr lst)))
  (if lst (setq! result (cons (car lst) result)))
  (reverse result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun reverse (lst)
 "Return a new list that is the reverse of the input list LST."
 (unless (list? lst) (error "LST must be a list"))
 (let ((result nil))
  (while lst
   (setq! result (cons (car lst) result))
   (setq! lst (cdr lst)))
  result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun butlast (lst)
 "Returns a new list that contains all the elements of the input list except the last one."
 (unless (list? lst) (error "LST must be a list"))
 (let ((result nil)
       (prev nil))
  (while (cdr lst)
   (setq! prev (cons (car lst) prev))
   (setq! lst (cdr lst)))
  (while prev
   (setq! result (cons (car prev) result))
   (setq! prev (cdr prev)))
  result))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun copy-list (lst)
 "Take a shallow copy of LST."
 (unless (list? lst) (error "LST must be a list"))
 (let ((result nil))
  (while lst
   (setq! result (cons (car lst) result))
   (setq! lst (cdr lst)))
  (reverse result)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defun-list-pred-fun (name combiner base-case)
 `(defun ,name (pred? lst)
   (if lst
    (,combiner
     (pred? (car lst))
     (,name pred? (cdr lst)))
    ,base-case)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun-list-pred-fun any? or  nil)
(defun-list-pred-fun all? and t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defun-list-transform-fun (name transformer)
 `(defun ,name (lsts)
   (when lsts
    (cons (,transformer (car lsts)) (,name (cdr lsts))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun-list-transform-fun heads caar)
(defun-list-transform-fun tails cdar)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'misc-list-funs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
