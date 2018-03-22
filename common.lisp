(defun -flatten (lists)
  (apply #'append lists))
(assert (equal (-flatten '((1 2 3) (4 5 6) (7)))
               '(1 2 3 4 5 6 7)))
(assert (equal (-flatten '((1 2 3) (4 (5 6)) (7)))
               '(1 2 3 4 (5 6) 7)))
(assert (equal (-flatten '(() () ()))
               '()))

(defun -join (l &optional (sep ""))
  (if (null (cdr l))
      (car l)
    (concatenate 'string
                 (car l)
                 sep
                 (-join (cdr l) sep))))
(assert (equal (-join '("a" "b" "c")) "abc"))
(assert (equal (-join '("a" "b" "c") ", ") "a, b, c"))


(defun -contains-p (item list)
  (member item list :test #'equal))

(setf (fdefinition '-map) #'mapcar)
(setf (fdefinition '-filter) #'remove-if-not)
(setf (fdefinition '-every-p) #'every)


(defun -first (pred list)
  (cond ((null list) nil)
        ((funcall pred (car list)) (car list))
        (t (-first pred (cdr list)))))

(assert (equal nil (-first #'identity nil)))
(assert (equal nil (-first #'identity '(nil nil nil nil))))
(assert (equal 1 (-first #'identity '(nil nil 1 nil 2 nil))))

(defun mappend (fn list)
  "Append the results of calling fn on each element of list.
  Like mapcon, but uses append instead of nconc."
  (apply #'append (mapcar fn list)))

(provide 'common)
