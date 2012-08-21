;;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: utilities.lisp

;;;; Macros

;;; We define iteration macros to match the book's pseudo-code.
;;; This could all be done with LOOP, but some users don't have
;;; the LOOP from the 2nd edition of "Common Lisp: the Language".
;;; We also define a general with-collection macro to build lists,
;;; accumulate sums, maximize, etc.

;;; Fix för att köra ACL 5.0
(require "aclwin")
(require :ffcompat)

;;; not compatible with allegor 6.0
;;;
;;;(defmacro while (test do &body body)
;;;  "Execute bidy while the test is true."
;;;  (assert (eq do 'do))
;;;  `(do () ((not ,test) nil) ,@body))

(defmacro repeat (&body body)
  "Syntax is (REPEAT exp ... UNTIL test)."
  (let* ((exps (butlast body 2))
	 (until (elt body (- (length body) 2)))
	 (test (elt body (- (length body) 1))))
    (assert (eq until 'until))
    `(do () (nil) ,@exps (when ,test (return nil)))))

(defmacro for-each (var in list do &body body)
  "Execute body for each element of list.  VAR can be a list or tree
  of variables, in which case the elements are destructured."
  (assert (eq in 'in)) (assert (eq do 'do))
  (typecase var
    (symbol `(dolist (,var ,list) ,@body))
    (cons (let ((list-var (gensym)))
	    `(dolist (,list-var ,list)
	       (destructuring-bind ,var ,list-var ,@body))))
    (t (error "~V is an illegal variable in (for each ~V in ~A ...)"
	      var list))))

(defmacro for (var = start to end do &body body)
  "Execute body with var bound to succesive integers."
  (cond ((eq var 'each) ; Allow (for each ...) instead of (for-each ...)
	 `(for-each ,= ,start ,to ,end ,do ,@body))
	(t (assert (eq = '=)) (assert (eq to 'to)) (assert (eq do 'do))
	   (let ((end-var (gensym "END")))
	     `(do ((,var ,start (+ 1 ,var)) (,end-var ,end))
		  ((> ,var ,end-var) nil)
		,@body)))))

(defmacro with-collection ((&optional (fn 'collect) (init 'nil) (op 'cons)
				      (final (if (eq op 'cons) 'nreverse
					       'identity)))
			   &body body)
  "Within the body each call to (collect x) will collect x in a list.
  The FN argument defaults to COLLECT, but you can rename it if you want.
  You can also specify a different kind of collection via optional args.
  Examples: (with-collection () (collect 1) (collect (+ 1 1))) => (1 2)
  (with-collection (sum 0 +) (dotimes (i 10) (sum i))) => 45
  (with-collection (maximize 0 max) ... (maximize (f x)) ...)"
  (let ((var (gensym "COLLECT")))
    `(let ((,var ,init))
       (flet ((,fn (x) (setf ,var (,op x ,var))))
	 ,@body
	 (,final ,var)))))

(defmacro do-file ((var file &key (by '#'read)) &body body)
  "Execute body with VAR bound to succesive expressions from FILE."
  `(map-file #'(lambda (,var) ,@body) ,file :read ,by))

;;; Lagt till cltl1:: för att köra ACL
(defmacro deletef (item sequence &rest keys &environment env)
  "Destructively delete item from sequence, which must be SETF-able."
  (multiple-value-bind (temps vals stores store-form access-form)
      (cltl1::get-setf-method sequence env)
    (assert (= (length stores) 1))
    (let ((item-var (gensym "ITEM")))
    `(let* ((,item-var ,item)
	    ,@(mapcar #'list temps vals)
	    (,(first stores) (delete ,item-var ,access-form ,@keys)))
      ,store-form))))

(define-modify-macro nconcf (list &rest lists) nconc)

(defmacro define-if-undefined (&rest definitions)
  "Use this to conditionally define functions, variables, or macros that
  may or may not be pre-defined in this Lisp.  This can be used to provide
  CLtL2 compatibility for older Lisps."
  `(progn
     ,@(mapcar #'(lambda (def)
		   (let ((name (second def)))
		     `(when (not (or (boundp ',name) (fboundp ',name)
				     (special-form-p ',name)
				     (macro-function ',name)))
		       ,def)))
	       definitions)))

;;;; List Utilities

(defun length>1 (list)
  "Is this a list of 2 or more elements?"
  (and (consp list) (cdr list)))

(defun length=1 (list)
  "Is this a list of exactly one element?"
  (and (consp list) (null (cdr list))))

(defun random-element (list)
  (nth (abs (random (length list))) list))

(defun mappend (fn &rest lists)
  "Apply fn to respective elements of list(s), and append results."
  (reduce #'append (apply #'mapcar fn lists) :from-end t))

(defun mapply (fn list)
  "Each element of list is a list of args; apply fn to it, collect results."
  (mapcar #'(lambda (args) (apply fn args)) list))

(defun starts-with (list element)
  "Is this a list that starts with the given element?"
  (and (consp list) (eq (first list) element)))

(defun last1 (list)
  "Return the last element of a list."
  (first (last list)))

(defun left-rotate (list)
  "Move the first element to the end of the list."
  (append (rest list) (list (first list))))

(defun right-rotate (list)
  "Move the last element to the front of the list."
  (append (last list) (butlast list)))

(defun transpose (list-of-lists)
  "Transpose a matrix represented as a list of lists.
  Example: (transpose '((a b c) (d e f))) => ((a d) (b e) (c f))."
  (apply #'mapcar #'list list-of-lists))

(defun reuse-cons (x y x-y)
  "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
      (cons x y)))

(defun mapcar-into (fn list)
  "Apply fn to each element of list, and replace result right into list."
  ;; Borrowed from Don Geddis and/or Matt Ginsberg
  (do ((items list (cdr items)))
      ((null items) list)
    (rplaca items (funcall fn (car items)))))

;;; An expression is a list consisting of a prefix operator followed by args,
;;; Or it can be a symbol, denoting an operator with no arguments.
;;; Expressions are used in Logic, and as actions for agents.

(defun make-exp (op &rest args) (cons op args))
(defun op (exp) (if (listp exp) (first exp) exp))
(defun args (exp) (if (listp exp) (rest exp) nil))
(defun arg1 (exp) (first (args exp)))
(defun arg2 (exp) (second (args exp)))

(defsetf args (exp) (new-value)
  `(setf (cdr ,exp) ,new-value))

(defun prefix->infix (exp)
  "Convert a prefix expression into infix notation."
  (cond ((atom exp) exp)
	((length=1 (args exp)) exp)
	(t (insert-between (op exp) (mapcar #'prefix->infix (args exp))))))

(defun insert-between (item list)
  "Insert item between every element of list."
  (if (or (null list) (length=1 list))
      list
    (list* (first list) item (insert-between item (rest list)))))

;;;; Functions for 2-dimensional points 

(defstruct (xy (:type list)) x y)

(defun xy-p (arg) 
  "Is the argument a 2-D point?"
  (and (consp arg) (= (length arg) 2) (every #'numberp arg)))

(defun @ (x y) "Create a 2-D point" (make-xy :x x :y y))

(defun xy-equal (p q) (equal p q))

(defun xy-add (p q)
  (@ (+ (xy-x p) (xy-x q)) (+ (xy-y p) (xy-y q))))

(defun xy-distance (p q)
  "The distance between two points."
  (sqrt (+ (square (- (xy-x p) (xy-x q)))
	   (square (- (xy-y p) (xy-y q))))))

(defun x+y-distance (p q)
  "The 'city block distance' between two points."
  (+ (abs (- (xy-x p) (xy-x q)))
     (abs (- (xy-y p) (xy-y q)))))

(defun xy-between (xy1 xy2 xy3)
  "Predicate; return t iff xy1 is between xy2 and xy3. Points are collinear."
  (and (between (xy-x xy1) (xy-x xy2) (xy-x xy3))
       (between (xy-y xy1) (xy-y xy2) (xy-y xy3))))

(defun rotate (o a b c d &aux (x (xy-x o)) (y (xy-y o)))
  (@ (+ (* a x) (* b y)) (+ (* c x) (* d y))))

(defun inside (l xmax ymax &aux (x (xy-x l)) (y (xy-y l)))
  (and (>= x 0) (>= y 0) (< x xmax) (< y ymax)))


;;;; Numeric Utilities

(defconstant infinity most-positive-single-float)
(defconstant minus-infinity most-negative-single-float)

(defun average (numbers)
  "Numerical average (mean) of a list of numbers."
  (/ (sum numbers) (length numbers)))

(defun running-average (avg new n)
  "Calculate new average given previous average over n data points"
  (/ (+ new (* avg n)) (1+ n)))

(defun square (x) (* x x))

(defun sum (numbers &optional (key #'identity))
  "Add up all the numbers; if KEY is given, apply it to each number first."
  (if (null numbers)
      0
      (+ (funcall key (first numbers)) (sum (rest numbers) key))))

(defun between (x y z)
  "Predicate; return t iff number x is between numbers y and z."
  (or (<= y x z) (>= y x z)))

(defun rms-error (predicted target)
  "Compute root mean square error between predicted list and target list"
  (sqrt (ms-error predicted target)))

(defun ms-error (predicted target &aux (sum 0))
  "Compute mean square error between predicted list and target list"
  (mapc #'(lambda (x y) (incf sum (square (- x y)))) predicted target)
  (/ sum (length predicted)))

(defun boolean-error (predicted target)
  (if (equal predicted target) 0 1))

(defun dot-product (l1 l2 &aux (sum 0)) ;;; dot product of two lists
  (mapc #'(lambda (x1 x2) (incf sum (* x1 x2))) l1 l2)
  sum)

(defun iota (n &optional (start-at 0))
  "Return a list of n consecutive integers, by default starting at 0."
  (if (<= n 0) nil (cons start-at (iota (- n 1) (+ start-at 1)))))

(defun random-integer (from to)
  "Return an integer chosen at random from the given interval."
  (+ from (random (+ 1 (- to from)))))

(defun normal (x mu sigma)
  (/ (exp (/ (- (square (- x mu))) (* 2 (square sigma)))) 
     (* (sqrt (* 2 pi)) sigma)))

(defun sample-with-replacement (n population)
  (let ((result nil))
    (dotimes (i n) (push (random-element population) result))
    result))

(defun sample-without-replacement (n population &optional
				     (m (length population)))
  ;; Assumes that m = (length population)
  (cond ((<= n 0) nil)
	((>= n m) population)
	((>= (/ n m) (random 1.0))
	 (cons (first population) (sample-without-replacement
				   (- n 1) (rest population) (- m 1))))
	(t (sample-without-replacement n (rest population) (- m 1)))))

(defun fuzz (quantity &optional (proportion .1) (round-off .01))
  "Add and also subtract a random fuzz-factor to a quantity."
  (round-off (+ quantity
		(* quantity (- (random proportion) (random proportion))))
	     round-off))

(defun round-off (number precision)
  (* precision (round number precision)))

;;;; Trivial Functions

(defun nothing (&rest args)
  (declare (ignore args))
  nil)

#-MCL ;; Macintosh Common Lisp already defines this function
(defun true (&rest args)
  (declare (ignore args))
  t)

#-MCL ;; Macintosh Common Lisp already defines this function
(defun false (&rest args)
  (declare (ignore args))
  nil)

(defun zero (&rest args)
  (declare (ignore args))
  0)

(defun one (&rest args)
  (declare (ignore args))
  1)

(defun required ()
  "If this ever gets called, it means something that was required was not
  supplied.  Use as default value for &key args or defstruct slots."
  (error "A required argument was not supplied."))


;;;; Utilities for strings and symbols and printing

(defun stringify (exp)
  (cond ((stringp exp) exp)
	((symbolp exp) (symbol-name exp))
	(t (format nil "~A" exp))))

(defun concat-symbol (&rest args)
  "Concatenate the args into one string, and turn that into a symbol."
  (intern (string-upcase (format nil "~{~a~}" args))))

(defun echo (&rest args)
  "Print the arguments, separated by blanks and followed by a newline."
  (format t "~&~{~A ~}~%" args))

(defun print-grid (array &key (stream t) (key #'identity) (width 3))
  "Print the contents of a 2-D array, numbering the edges."
  (let ((max-x (- (array-dimension array 0) 1))
	(max-y (- (array-dimension array 1) 1)))
    ;; Print the header
    (format stream "~&") (print-repeated " " width stream)
    (for x = 0 to max-x do
	 (format stream "!") (print-dashes width stream))
    (format stream "!~%")
    ;; Print each row
    (for y1 = 0 to max-y do
	 (let ((y (- max-y y1)))
	   (print-centered y width stream)
	   ;; Print each location
	   (for x = 0 to max-x do
		(format stream "!")
		(print-centered (funcall key (aref array x y)) width stream))
	   (format stream "!~%") 
	   ;; Print a dashed line
	   (print-repeated " " width stream)
	   (for x = 0 to max-x do
		(format stream "!") (print-dashes width stream)))
	 (format stream "!~%"))
    ;; Print the X-coordinates along the bottom
    (print-repeated " " width stream)
    (for x = 0 to max-x do
	 (format stream " ") (print-centered x width stream))
    array))

(defun print-centered (string width &optional (stream t))
  "Print STRING centered in a field WIDTH wide."
  (let ((blanks (- width (length (stringify string)))))
    (print-repeated " " (floor blanks 2) stream)
    (format stream "~A" string)
    (print-repeated " " (ceiling blanks 2) stream)))

(defun print-repeated (string n &optional (stream t))
  (dotimes (i n)
    (format stream "~A" string)))

(defun print-dashes (width &optional (stream t) separate-line)
  (when separate-line (format stream "~&"))
  (print-repeated "-" width stream)
  (when separate-line (format stream "~%")))

;;;; Other

(defun map-file (fn file &key (read #'read))
  "Apply fn to every expression in file."
  (with-open-file (s file :direction :input)
    (let ((eof "EOF"))
      (do ((x (funcall read s nil eof) (funcall read s nil eof)))
	  ((eq x eof) nil)
        (funcall fn x)))))

(defun copy-array (a &aux (dim (array-dimensions a))
                          (b (make-array dim)))
  "Make a copy of an array."
  (copy-subarray a b nil dim)
  b)

(defun copy-subarray (a b indices dim)
  (if dim
    (dotimes (i (first dim))
      (copy-subarray a b (append indices (list i)) (rest dim)))
    (setf (apply #'aref (cons b indices))
          (apply #'aref (cons a indices)))))

(defun array->vector (array)
  "Convert a multi-dimensional array to a vector with the same elements."
  (make-array (array-total-size array) :displaced-to array))


(defun plot-alist (alist file)
  (with-open-file (stream file :direction :output :if-does-not-exist :create
                     :if-exists :supersede)
    (dolist (xy alist)
      (format stream "~&~A ~A~%" (car xy) (cdr xy)))))

(defun copy-hash-table (H1 &optional (copy-fn #'identity))
  (let ((H2 (make-hash-table :test #'equal)))
    (maphash #'(lambda (key val) (setf (gethash key H2) (funcall copy-fn val)))
	     H1)
    H2))

(defun hprint (h &optional (stream t)) "prints a hash table line by line"
  (maphash #'(lambda (key val) (format stream "~&~A:~10T ~A" key val)) h)
  h)

(defun compose (f g)
  "Return a function h such that (h x) = (f (g x))."
  #'(lambda (x) (funcall f (funcall g x))))

(defun the-biggest (fn l)
  (let ((biggest (first l))
	(best-val (funcall fn (first l))))
    (dolist (x (rest l))
      (let ((val (funcall fn x)))
	(when (> val best-val)
	  (setq best-val val)
	  (setq biggest x))))
    biggest))

(defun the-biggest-random-tie (fn l)
  (random-element
   (let ((biggest (list (first l)))
	 (best-val (funcall fn (first l))))
     (dolist (x (rest l))
       (let ((val (funcall fn x)))
	 (cond ((> val best-val)
		(setq best-val val)
		(setq biggest (list x)))
	       ((= val best-val)
		(push x biggest)))))
     biggest)))

(defun the-biggest-that (fn p l)
  (let ((biggest (first l))
	(best-val (funcall fn (first l))))
    (dolist (x (rest l))
      (when (funcall p x)
	(let ((val (funcall fn x)))
	  (when (> val best-val)
	    (setq best-val val)
	    (setq biggest x)))))
    biggest))

(defun the-smallest (fn l)
  (the-biggest (compose #'- fn) l))

(defun the-smallest-random-tie (fn l)
  (the-biggest-random-tie (compose #'- fn) l))

(defun the-smallest-that (fn p l)
  (the-biggest-that (compose #'- fn) p l))

;;;; Debugging tool

(defvar *debugging* nil)

(defun dprint (&rest args)
  (when *debugging* (format t "~&~{~S ~}~%" args))
  (first args))

(defun dprinc (&rest args)
  (when *debugging* (format t "~{~S~^ ~}" args))
  (first args))

(defun dformat (format-string &rest args)
  (when *debugging*
    (apply #'format t format-string args)))

;;;; Testing Tool

(defmacro deftest (name &rest examples)
  "Define a set of test examples.  Each example is of the form (exp test)
  or (exp).  Evaluate exp and see if the result passes the test. Within the
  test, the result is bound to *.  The example ((f 2))) has no test to
  fail, so it alweays passes the test.  But ((+ 2 2) (= * 3)) has the test
  (= * 3), which fails because * will be bound to the result 4, so the test
  fails.  Call (TEST name) to count how many tests are failed within the
  named test.  NAME is the name of an aima-system."
  `(add-test ',name ',examples))

(defun add-test (name examples)
  "The functional interface for deftest: adds test examples to a system."
  (let ((system (or (get-aima-system name)
		    (add-aima-system :name name :examples examples))))
    (setf (aima-system-examples system) examples))
  name)

(defun test (&optional (name 'all) (print? 't))
  "Run a test suite and sum the number of errors.  If all is well, this
  should return 0.  The second argument says what to print: nil for
  nothing, t for everything, or FAIL for just those examples that fail.
  If there are no test examples in the named system, put the system has
  other systems as parts, run the tests for all those and sum the result."
  (let ((*print-pretty* t)
	(system (get-aima-system name)))
    (cond ((null system) (warn "No tests defined for ~A." name))
	  ((and (null (aima-system-examples system))
		(every #'symbolp (aima-system-parts system)))
	   (sum  (aima-system-parts system)
		 #'(lambda (part) (test part print?))))
          (t (count-if-not #'(lambda (example) (test-example example print?))
			   (aima-system-examples system))))))

(defun test-example (example &optional (print? t))
  "Does the EXP part of this example pass the TEST?"
  (if (stringp example)
      (progn
        (when (eq print? t)
          (format t "~&;;; ~A~%" example))
        t)
    (let* ((exp (first example))
	   *
	   (test (cond ((null (second example)) t)
		       ((constantp (second example))
			`(equal * ,(second example)))
		       (t (second example))))
           test-result)
      (when (eq print? t)
        (format t "~&> ~S~%" exp))
      (setf * (eval exp))
      (when (eq print? t)
        (format t "~&~S~%" *))
      (setf test-result (eval test))
      (when (null test-result)
        (case print?
          ((FAIL) (format t "~&;;; FAILURE on ~S; expected ~S, got:~%;;; ~S~%"
                          exp test *))
          ((T) (format t "~&;;; FAILURE: expected ~S" test))
          (otherwise)))
      test-result)))
  
#|| An example:
(deftest math
    "A test suite"    ; comment
   ((+ 2 2))          ; example with no test
   ((+ 2 2) (= * 4))  ; test succeeds
   ((+ 2 2) '4)       ; test succeeds; equivalent to previous test
   ((+ 3 3) (= * 5))) ; test fails
||#

