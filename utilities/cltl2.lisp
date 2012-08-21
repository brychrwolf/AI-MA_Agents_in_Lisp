;;; File: cltl2.lisp -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;;; CLtL2 Compatibility package

;;; Functions and macros in "Common Lisp the Language: 2nd edition"
;;; that are not in the first edition, and thus not in some old
;;; implementations of Common Lisp.

#+Allegro ;; Allow us to create missing functions in Allegro 
(when (fboundp 'package-definition-lock)
  (setf (package-definition-lock (find-package "COMMON-LISP")) nil))

(define-if-undefined
  
  (defmacro WITH-SIMPLE-RESTART (restart &rest body)
    (declare (ignore restart))
    `(progn ,@body))

  (defmacro DESTRUCTURING-BIND (lambda-list list &body body)
    "Bind the variables in lambda-list to the result list and execute body."
    ;; This implementation does not do the defmacro extensions,
    ;; Except that it does handle a trailing dot: (x y . z)
    (cond ((null lambda-list)
	   `(progn ,@body))
	  ((not (symbolp list))
	   (let ((var (gensym)))
	     `(let ((,var ,list))
		(destructuring-bind ,lambda-list ,var ,@body))))
	  ((symbolp lambda-list)
	   `(let ((,lambda-list ,list)) ,@body))
	  ((atom lambda-list)
	   (error "Can't bind ~A to a value." lambda-list))
	  ((member (first lambda-list) '(&rest &optional &key &aux))
	   `(apply #'(lambda ,lambda-list ,@body) ,list))
	  (t `(destructuring-bind ,(first lambda-list) (first ,list)
		(destructuring-bind ,(rest lambda-list) (rest ,list)
		  ,@body)))))

  
  (defmacro PRINT-UNREADABLE-OBJECT ((object stream &key type identity)
				     &body body)
    ;; Taken from Xerox's CLOSETTE source code.
    (let ((stream. (gensym))
	  (object. (gensym)))
      `(let ((,stream. ,stream)
	     (,object. ,object))
	 (write-char #\# ,stream.)
	 (write-char #\< ,stream.)
	 ,@(when type `((write (type-of ,object.) :stream stream)))
	 ,@(when (and type (or body identity))
	     `((write-char #\Space ,stream.)))
	 ,@body
	 ,@(when (and identity body) `((write-char #\Space ,stream.)))
	 ,@(when identity
	     #+Lispworks `((format ,stream. "~O"
				   (system:object-pointer ,object.)))
	     #+Genera `((format ,stream. "~O" (si:%pointer ,object.)))
	     #+Lucid  `((format ,stream. "~O" (sys:%pointer ,object.)))
	     #+Excl   `((format ,stream. "~O"
				(excl::pointer-to-fixnum ,object.)))
	     #+:coral `((format ,stream. "~O" (ccl::%ptr-to-int ,object.)))
	     #+CLISP  `((format ,stream. "#x~6,'0X"
				(logand (sys::address-of ,object.) #xFFFFFF))
			)
	     )
	 (write-char #\> ,stream.)
	 nil)))

  (defun CONSTANTLY (value)
    "Return the function that always returns VALUE."
    #'(lambda (&rest args) (declare (ignore args)) value))

  ) ; end define-if-undefined
