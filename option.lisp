
;; Decided not to use this for gps because the option logic overwhelmed the program!

(defun make-option (set x)
  (if set (make-just x) (make-none)))

(defun make-none ()
  '(option none))

(defun make-just (x)
  `(option just ,x))

(defun option-p (x)
  (equal (first x) 'option))

(defun none-p (x)
  (assert (option-p x))
  (equal (second x) 'none))

(defun just-p (x)
  (assert (option-p x))
  (equal (second x) 'just))

(defun option-get (x)
  (assert (just-p x))
  (third x))

(defun option-map (f x)
  (assert (option-p x))
  (if (none-p x)
      x
    (make-just (funcall f (option-get x)))))

(assert (option-p (make-none)))
(assert (option-p (make-just 'hi)))
(assert (equal (option-get (make-just 'hi)) 'hi))
(assert (equal (option-get (make-just nil)) nil))

(assert (just-p (make-just 'hi)))
(assert (not (none-p (make-just 'hi))))

(assert (none-p (make-none)))
(assert (not (just-p (make-none))))

(assert (just-p (make-option t 'hi)))
(assert (none-p (make-option nil 'hi)))

(assert (equal (option-map #'1+ (make-just 1)) (make-just 2)))
(assert (equal (option-map #'1+ (make-none)) (make-none)))


(defun option-all (options)
  (assert (-every-p #'option-p options))
  (if (-every-p #'just-p options)
      (make-just (-map #'option-get options))
    (make-none)))

(assert (equal (option-all (list (make-just 1) (make-just 2) (make-just 3)))
               (make-just '(1 2 3))))
(assert (equal (option-all (list (make-just 1) (make-none) (make-just 3)))
               (make-none)))
(assert (equal (option-all nil) (make-just nil)))
