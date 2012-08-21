;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: utilities/test.lisp

(deftest utilities
  "Test the macros"
  ((with-collection () (for i = 1 to 5 do (collect i)))
   (equal * '(1 2 3 4 5)))
  ((with-collection (summing 0 +) (for i = 1 to 5 do (summing i)))
   (equal * 15))
  ((with-collection () (for each x in '(a b) do (collect x)))
   (equal * '(a b)))
  ((with-collection () (setq i 1) (while (< i 5) do (collect i) (incf i)))
   (equal * '(1 2 3 4)))
  ((with-collection () (setq i 1) (repeat (collect i) (incf i) until (> i 5)))
   (equal * '(1 2 3 4 5)))
  "First, some operations on lists."
  ((length>1 '(a b c)) *)
  ((random-element '(a b c)) (member * '(a b c)))
  ((mappend #'reverse '((a b c) (1 2 3))) (equal * '(c b a 3 2 1)))
  ((starts-with '(hi there) 'hi) *)
  ((last1 '(a b c)) (eq * 'c))
  ((transpose '((a b c) (d e f))) (equal * '((a d) (b e) (c f))))
  ((setq l '(a b c)))
  ((deletef 'a l) (equal l '(b c)))
  "Now for 2-dimensional points."
  ((xy-add (@ 1 2) (@ 10 20)) (equal * (@ 11 22)))
  ((xy-distance (@ 0 0) (@ 3 4)) (= * 5))
  "Numeric utilities"
  ((average '(10 20 30)) (= * 20))
  ((sum '(10 20 30)) (= * 60))
  ((sum '(1 2 3) #'square) (= * 14))
  ((random-integer 8 10) (member * '(8 9 10)))
  ((fuzz 10) (<= 9 * 11))
  ((round-off 3.14159 .01) (< 3.139 * 3.141))
  "Other"
  ((stringify '(a b c)) (equalp * "(A B C)"))
  ((concat-symbol 'a 1) (eq * 'a1))
  ((funcall (compose #'- #'sqrt) 16) (= * -4))
  ((setq nums '(1 2 3 4 -5 -2 -1)))
  ((the-biggest #'identity nums) (eql * 4))
  ((the-biggest #'abs nums) (eql * -5))
  ((the-biggest-that #'identity #'oddp nums) (eql * 3))
  ((the-smallest-random-tie #'abs nums) (member * '(1 -1)))
  "Now test the priority queue code."
  ((heap-sort '(1 4 3 5 2 0)) (equal * '(0 1 2 3 4 5)))
  ((heap-sort '(1 4 3 5 2 6) :key #'-) (equal * '(6 5 4 3 2 1)))
  "Now destructuring-bind"
  ((destructuring-bind ((a . b) c &rest d &key e (f 5)) '((1 . 2) 3 :e 4)
     (list a b c d e f)) (equal * '(1 2 3 (:e 4) 4 5)))
  )



