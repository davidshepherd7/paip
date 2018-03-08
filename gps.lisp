(require 'arrows "./arrows.lisp")



(defun -flatten (lists)
(apply #'append lists))
(assert (equal (-flatten '((1 2 3) (4 5 6) (7)))
            '(1 2 3 4 5 6 7)))
(assert (equal (-flatten '((1 2 3) (4 (5 6)) (7)))
            '(1 2 3 4 (5 6) 7)))
(assert (equal (-flatten '(() () ()))
            '()))

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



(defstruct op "An operation"
           (action nil) (preconds nil) (add-list nil) (del-list nil))

(defvar *ops* nil "A list of available operators.")


(defvar op-shop-installs-battery
  (make-op :action 'shop-installs-battery
           :preconds '(car-needs-battery shop-knows-problem shop-has-money)
           :add-list '(car-works)))

(defparameter *school-ops*
  (list
    (make-op :action 'drive-son-to-school
         :preconds '(son-at-home car-works)
         :add-list '(son-at-school)
         :del-list '(son-at-home))
    op-shop-installs-battery
    (make-op :action 'tell-shop-problem
         :preconds '(in-communication-with-shop)
         :add-list '(shop-knows-problem))
    (make-op :action 'telephone-shop
         :preconds '(know-phone-number)
         :add-list '(in-communication-with-shop))
    (make-op :action 'look-up-number
         :preconds '(have-phone-book)
         :add-list '(know-phone-number))
    (make-op :action 'give-shop-money
         :preconds '(have-money)
         :add-list '(shop-has-money)
         :del-list '(have-money))))

(defun achieves-goal-p (goal op)
  (-contains-p goal (op-add-list op)))

(setf *ops* *school-ops*)


(defun action-p (thing)
  (and (listp thing)
       (or (equal thing '(start))
           (equal (first thing) 'executing))))


(defun sibling-clobber-p (state goals)
  (when (subsetp goals state)
    state))

(defun gps (state goals &optional (*ops* *ops*)) ; -> States
  (-<> (cons '(start) state)
       (achieve-all <> goals)
       (sibling-clobber-p <> goals)
       (-filter #'action-p <>)))


(defun achieve-all (state goals) ; -> States
  (if (null goals)
      state
    (let ((updated-state (achieve state (first goals))))
      (when updated-state
        (achieve-all updated-state (rest goals))))))


(defun achieve (state goal) ; -> States
  (if (-contains-p goal state)
      state
    (-<> *ops*
         (-filter #'(lambda (op) (achieves-goal-p goal op)) <>)
         (-map #'(lambda (op) (state-after-achieved state op)) <>)
         (-first #'identity <>))))


(defun state-after-achieved (state op) ; -> States
    (let ((updated-state (achieve-all state (op-preconds op))))
      (when updated-state
        (update-state updated-state op))))


(defun update-state (state op) ; -> States
  (append
   (-filter #'(lambda (s) (not (-contains-p s (op-del-list op)))) state)
   (list (list 'executing (op-action op)))
   (-filter #'(lambda (s) (not (-contains-p s state))) (op-add-list op))))


(assert (equal (update-state '(a b c) (make-op :action 'foo
                                               :preconds '(a b)
                                               :add-list '(x y a)
                                               :del-list '(b)))
               '(a c (executing foo) x y)))

;; trivial example
(assert (equal (gps '(son-at-home) '(son-at-home)) '((start))))

;; simple
(assert (equal (gps '(son-at-home car-works)
                    '(son-at-school))
               '((start)
                 (executing drive-son-to-school))))

;; non trivial
(assert (equal (gps '(son-at-home car-needs-battery have-money have-phone-book)
                    '(son-at-school))
               '((start)
                 (executing look-up-number)
                 (executing telephone-shop)
                 (executing tell-shop-problem)
                 (executing give-shop-money)
                 (executing shop-installs-battery)
                 (executing drive-son-to-school))))

;; Sibling clobbering
(assert (null (gps '(son-at-home car-needs-battery have-money have-phone-book)
                   '(have-money son-at-school))))
(assert (null (gps '(son-at-home car-needs-battery have-money have-phone-book)
                   '(son-at-school have-money))))
(assert (null (gps '(son-at-home car-needs-battery have-money)
                   '(son-at-school))))
