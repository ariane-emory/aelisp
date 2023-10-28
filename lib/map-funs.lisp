;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list funs (map variants):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mapcar (fun lst)
 "Map fun over LST, returning the resulting list."
 (unless (fun? fun)   (error "FUN must be a function"))
 (unless (list? lst)  (error "LST must be a list"))
 (let ((acc nil))
  (while lst
   (setq! acc (cons (fun (car lst)) acc))
   (setq! lst    (cdr lst)))
  (reverse acc)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mapcar* (fun . args) (apply mapcar fun (list args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mapcar! (fun lst)
 "Map fun over LST, altering the list."
 (unless (fun? fun)  (error "FUN must be a function"))
 (unless (list? lst) (error "LST must be a list"))
 (let ((current lst))
  (while current
   (setcar! current (fun (car current)))
   (setq! current (cdr current)))
  lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mapconcat (fun lst . rest)
 "Map fun over LST, returning the result of concatenating the resulting strings."
 (unless (fun? fun)     (error "FUN must be a function"))
 (unless (list? lst)    (error "LST must be a list"))
 (unless (or (nil? rest) (single? rest))
  (error "MAPCONCAT takes exactly only one optional arguments after LST"))
 (let ((delimiter (car rest))
       (acc (if lst (fun (car lst)) ""))
       (current (cdr lst)))
  (unless (or (nil? delimiter) (string? delimiter))
   (error "DELIMITER must be a string or nil"))
  (while current
   (setq acc (concat acc (or delimiter "") (fun (car current))))
   (setq current (cdr current)))
  acc))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mapcan (fun lst)
 "Map fun over LST and concatenate the results by altering them."
 (unless (fun? fun)   (error "FUN must be a function"))
 (unless (list? lst)  (error "LST must be a list"))
 (let ((results nil)
       (tail nil)
       (current lst))
  (while current
   (let ((result (fun (car current))))
    (when result
     (if results
      (progn
       (setq! tail (nconc! tail result))
       (setq! tail (last tail)))
      (setq! results result)
      (setq! tail results))))
   (setq! current (cdr current)))
  results))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mapc (fun lst)
 "Apply FUN to each element of LST for side effects only and return LST."
 (unless (fun? fun)   (error "FUN must be a function"))
 (unless (list? lst)  (error "LST must be a list"))
 (let ((current lst))
  (while current
   (fun (car current))
   (setq! current (cdr current)))
  lst))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mapc* (fun . args) (apply mapc fun (list args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'map-funs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
