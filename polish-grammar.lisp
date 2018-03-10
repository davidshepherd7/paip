
(defun mappend (fn list)
  "Append the results of calling fn on each element of list.
  Like mapcon, but uses append instead of nconc."
  (apply #'append (mapcar fn list)))

(defun random-elt (seq)
  "Pick a random element out of a sequence."
  (elt seq (random (length seq))))

(defun join (seq sep)
  (if (null (rest seq))
      (first seq)
    (concatenate 'string (first seq) sep (join (rest seq) sep))))

(defun adjective-noun ()
  (let ((noun (first (generate 'Noun)))
        (adjectives (generate 'Adj*)))
    (append
     (mapcar #'(lambda (adj) (genderise-adjective adj (noun-gender noun))) adjectives)
     (list noun))))


(defparameter *polish-grammar* nil)
(setf *polish-grammar*
      `((sentence -> (noun-phrase verb-phrase))
        (noun-phrase -> ,#'adjective-noun (Name) (Pronoun))
        (verb-phrase -> (Verb noun-phrase))
        (Adj* -> () (Adj Adj*))
        (Adj -> ,"duży" ,"mały" ,"niebiesky" ,"zielony")
        (Name -> ,"Sylwia" ,"Dawid" ,"Bogdan")
        (Verb -> ,"uderzać" ,"brać" ,"widzieć" ,"lubić")
        (Noun -> ,"piłka" ,"kobieta" ,"stół" ,"dziecko")
        (Pronoun -> ,"on" ,"ona" ,"tam")))

(defvar *grammar* *polish-grammar*)
(setf *grammar* *polish-grammar*)

(defun rule-lhs (rule)
  "The left hand side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

(defun generate (phrase)
  "Generate a random sentence or phrase"
  (cond ((functionp phrase)
         (generate (funcall phrase)))
        ((listp phrase)
         (mappend #'generate phrase))
        ((stringp phrase)
         (list phrase))
        ((rewrites phrase)
         (generate (random-elt (rewrites phrase))))
        (t (error "Unknown thingy ~S" phrase))))

(defun sentence-case (str)
  (concatenate 'string
               (string-upcase (subseq str 0 1))
               (subseq str 1)))

(defun generate-sentence ()
  (concatenate 'string
               (sentence-case (join (generate 'sentence) " "))
               "."))

(defun generate-paragraph ()
  ;; TODO make this not stupid
  (join (mapcar #'(lambda (_) (generate-sentence))
                '(1 2 3 4 5 6 7 8 9 10))
        " "))



(defun ends-with-p (suffix s)
  "Determine whether `s` ends with `suffix`"
  (let ((p (mismatch suffix s :from-end T)))
    (or (not p) (= 0 p))))

(assert (ends-with-p "" "arostienx"))
(assert (ends-with-p "x" "arostienx"))
(assert (ends-with-p "nx" "arostienx"))
(assert (ends-with-p "enx" "arostienx"))
(assert (not (ends-with-p "ar" "arostienx")))
(assert (not (ends-with-p "araorsitnoarsn" "n")))
(assert (not (ends-with-p "araorsitnoarsn" "aso")))


(defun noun-gender (noun)
  "Determine the gender of noun in Polish. Ignores special cases like mężczyzna."
  (cond
   ((ends-with-p "a" noun) 'fem)
   ((or (ends-with-p "o" noun) (ends-with-p "e" noun)) 'neut)
   (t 'masc)))

(assert (equal (noun-gender "kobieta") 'fem))
(assert (equal (noun-gender "dziecko") 'neut))
(assert (equal (noun-gender "stoł") 'masc))


(defun chop-suffix (suffix s)
  "Stolen from s.el"
  (let ((pos (- (length s) (length suffix))))
    (if (and (>= (length s) (length suffix))
             (string= suffix (subseq s pos)))
        (subseq s 0 pos)
      s)))

(defun adjective-stem (masc-adjective)
  (cond ((ends-with-p "y" masc-adjective)
         (chop-suffix "y" masc-adjective))
        ((ends-with-p "i" masc-adjective)
         (chop-suffix "i" masc-adjective))
        (t (error "Unrecognised ending of masculine adjective: ~S" masc-adjective))))

(defun genderise-adjective (masc-adjective gender)
  (let ((ending (cond ((ends-with-p "y" masc-adjective)
                       (ecase gender
                         (fem  "ą")
                         (masc "y")
                         (neut "e")))
                      ((ends-with-p "i" masc-adjective)
                       (ecase gender
                         (fem  "a")
                         (masc "i")
                         (neut "ie")))
                      (t (error "Unrecognised masculine adjective ending in ~S" masc-adjective)))))
    (concatenate 'string (adjective-stem masc-adjective) ending)))

(assert (equal (genderise-adjective "mały" 'fem) "małą"))
(assert (equal (genderise-adjective "długi" 'fem) "długa"))


