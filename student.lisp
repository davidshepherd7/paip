(require 'arrows "./arrows.lisp")
(require 'common "./common.lisp")

(defstruct (rule (:type list)) pattern response)

(defstruct (alg-exp (:type list)
                    (:constructor mkexp (lhs op rhs)))
  op lhs rhs)

(defun alg-exp-p (x) (consp x))
(defun alg-exp-args (x) (rest x))

(defun prefix->infix (exp)
  (cond
   ((atom exp) exp)
   (t (-map #'prefix->infix
            (list (alg-exp-lhs exp)
                  (alg-exp-op exp)
                  (alg-exp-rhs exp))))))

(defun format-equation (exp)
  (format nil "~a" (prefix->infix exp)))

(defun print-equations (equations)
  (-<> equations
       (-map #'format-equation <>)
       (-join <>)
       (format nil "~a~%" <>)
       (princ <>)))

(defun unknown-p (exp)
  (and (symbolp exp)
       (not (member exp '(+ - * /)))))

(defun count-unknowns (exp)
  (cond
   ((unknown-p exp) 1)
   ((atom exp) 0)
   (t (+ (count-unknowns (alg-exp-lhs exp))
         (count-unknowns (alg-exp-rhs exp))))))

(assert (equal (count-unknowns '(= x (+ 2 (* 4 5)))) 1))
(assert (equal (count-unknowns '(= 10 (+ 2 (* 4 5)))) 0))
(assert (equal (count-unknowns '(= 10 (+ 2 (* 4 x)))) 1))
(assert (equal (count-unknowns '(= (* 4 (/ 3 4)) (/ 2 ))) 1))

(defun no-unknown (exp) (= 0 (count-unknowns exp)))
(defun one-unknown (exp) (= 1 (count-unknowns exp)))

(defun solve-arithmatic (exp)
  (assert (equal (first exp) '=))
  (assert (no-unknown (alg-exp-rhs exp)))
  (mkexp (alg-exp-lhs exp) '= (eval (alg-exp-rhs exp))))
(assert (equal (solve-arithmatic '(= x (+ 2 (* 3 (/ 10 2)))))
               '(= x 17)))

(defconstant *inverses*
  '((+ -) (- +) (= =) (* /) (/ *)))

(defun inverse-op (op)
  (second (or (assoc op *inverses*)
              (error "No such operator ~a" op))))

(defun isolate (exp)
  (assert (one-unknown exp))
  (cond
   ;; We're done
   ((or (unknown-p exp)
        (unknown-p (alg-exp-rhs exp))
        (unknown-p (alg-exp-lhs exp))) exp)

   ;; Flip the expression so that the unknown is alway on the lhs
   ((one-unknown (alg-exp-rhs exp)) (isolate (mkexp (alg-exp-rhs exp)
                                                    (alg-exp-op exp)
                                                    (alg-exp-lhs exp))))

   ((one-unknown (alg-exp-lhs exp))
    (if (commutative-p op)

    ;; TODO: this is slow
    (let ((unknown (if (one-unknown (alg-exp-lhs (alg-exp-lhs exp)))
                       (alg-exp-lhs (alg-exp-lhs exp))
                     (alg-exp-rhs (alg-exp-lhs exp))))
          (other (if (one-unknown (alg-exp-lhs (alg-exp-lhs exp)))
                     (alg-exp-rhs (alg-exp-lhs exp))
                   (alg-exp-lhs (alg-exp-lhs exp)))))

      (isolate (mkexp unknown
                      (alg-exp-op exp)
                      (mkexp (alg-exp-rhs exp)
                             (inverse-op (alg-exp-op (alg-exp-lhs exp)))
                             other))))

    ; else?
    )

   (t (error "Failed to parse ~a" exp))))

(trace isolate)
(trace one-unknown)

(assert (equal (isolate 'x) 'x))
(assert (equal (isolate '(= x 1)) '(= x 1)))
(assert (equal (isolate '(= x (+ 1 (* 2 3)))) '(= x (+ 1 (* 2 3)))))

(assert (equal (isolate '(= (+ x 2) 10)) '(= x (- 10 2))))
(assert (equal (isolate '(= 10 (+ x 2))) '(= x (- 10 2))))
(assert (equal (isolate '(= 10 (+ 2 x))) '(= x (- 10 2))))

(assert (equal (isolate '(= 10 (* 2 x))) '(= x (/ 10 2))))
(assert (equal (isolate '(= 10 (* x 2))) '(= x (/ 10 2))))

(assert (equal (isolate '(= 10 (/ 2 x))) '(= x (/ 2 10))))
(assert (equal (isolate '(= 10 (/ x 2))) '(= x (* 10 2))))




;; TODO /, x in nested equations
