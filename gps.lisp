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

(defun mappend (fn list)
  "Append the results of calling fn on each element of list.
  Like mapcon, but uses append instead of nconc."
  (apply #'append (mapcar fn list)))



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

(setf *ops* *school-ops*)


(defun achieves-goal-p (goal op)
  (-contains-p goal (op-add-list op)))


(defun action-p (thing)
  (and (listp thing)
       (or (equal thing '(start))
           (equal (first thing) 'executing))))


(defun sibling-clobber-p (state goals)
  (when (subsetp goals state :test #'equal)
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



(defun make-maze-ops (pair)
  "Make maze ops in both directions"
  (list (make-maze-op (first pair) (second pair))
        (make-maze-op (second pair) (first pair))))

(defun make-maze-op (here there)
  "Make an operator to move between two places"
  (make-op :action `(move from ,here to ,there)
           :preconds `((at ,here))
           :add-list `((at ,there))
           :del-list `((at ,here))))

(defparameter *maze-ops*
  (mappend #'make-maze-ops
           '((1 2) (2 3) (3 4) (4 9) (9 14) (9 8) (8 7) (7 12) (12 13)
             (12 11) (11 6) (11 16) (16 17) (17 22) (21 22) (22 23)
             (23 18) (23 24) (24 19) (19 20) (20 15) (15 10) (10 5) (20 25))))

;; TODO: abort cycles so that this works

;; (gps '((at 1)) '((at 25)) *maze-ops*)



(defun make-block-ops (blocks)
  (let ((ops nil))
    (dolist (a blocks)
      (dolist (b blocks)
        (unless (equal a b)
          (dolist (c blocks)
            (unless (or (equal c a) (equal c b))
              (push (move-op a b c) ops)))
          (push (move-op a 'table b) ops)
          (push (move-op a b 'table) ops))))
    ops))

(defun move-op (a b c)
  "Make an operator to move A from B to C."
  (make-op :action `(move ,a from ,b to ,c)
           :preconds `((space on ,a) (space on ,c) (,a on ,b))
           :add-list (move-ons a b c)
           :del-list (move-ons a c b)))

(defun move-ons (a b c)
  (if (eq b 'table)
      `((,a on ,c))
    `((,a on ,c) (space on ,b))))

(setf *ops* (make-block-ops '(a b)))

(assert (equal (gps '((a on table) (b on table) (space on a) (space on b) (space on table))
                    '((a on b) (b on table)))
               '((start) (executing (move a from table to b)))))

(assert (equal (gps '((a on b) (b on table) (space on a) (space on table))
                    '((b on a)))
               '((start)
                 (executing (move a from b to table))
                 (executing (move b from table to a)))))

;; TODO: try different orderings so that this works

;; (assert (equal (gps '((a on b) (b on c) (c on table) (space on a) (space on table))
;;                     '((b on a) (c on b)))
;;                '((start)
;;                  (executing (move a from b to table))
;;                  (executing (move b from c to a))
;;                  (executing (move c from table to b)))))

;; (assert (equal (gps '((c on a) (a on table) (b on table)
;;                       (space on c) (space on b) (space on table))
;;                     '((c on table) (a on b)))
;;                '((start)
;;                  (executing (move c from a to table))
;;                  (executing (move a from table to b)))))

;; (assert (equal (gps '((a on b) (b on c) (c on table) (space on a) (space on table))
;;                     '((b on a) (c on b)))
;;                '((start)
;;                  (executing (move a from b to table))
;;                  (executing (move b from c to a))
;;                  (executing (move c from table to b)))))

;; (assert (equal (gps '((a on b) (b on c) (c on table) (space on a) (space on table))
;;                     '((c on b) (b on a)))
;;                '((start)
;;                  (executing (move a from b to table))
;;                  (executing (move b from c to a))
;;                  (executing (move c from table to b)))))


;; ((use (push (op 'taxi-son-to-school
;;                :preconds '(son-at-home have-money)
;;                :add-list '(son-at-school)
;;                :del-list '(son-at-home have-money))
;;            *school-ops*)))
;; ((gps '(son-at-home have-money car-works)
;;        '(son-at-school have-money))
