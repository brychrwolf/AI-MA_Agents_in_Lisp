;;; Load garnet

;(load "/home/HKGBB0/garnet/garnet-loader")
(load "/home/TDDA23/garnet/garnet-loader")

;;; Package definition

(defpackage agent-gui
  (:use "KR" "CL")
  (:export init-gui update-gui destroy-gui))

(in-package agent-gui)


;;; MACROS FOR CREATION OF WINDOW, AGENT, WORLD AND OBJECTS

(defmacro create-window ()
  `(progn (create-instance 'AGENT-WIN inter:interactor-window
			   (:left 10) (:top 10) 
			   (:width 500) (:height 500)
			   (:double-buffered-p t))
	  (create-instance 'TOP-AGG opal:aggregate)
	  (s-value AGENT-WIN :aggregate TOP-AGG)))

(defmacro create-agent ()
  `(progn 
     (create-instance 'AGENT-AGG opal:aggregadget
		      (:box '(0 450 48 48))
		      (:dir '(nil nil nil t))
		      (:parts `((head ,opal:circle
				      (:width 30)
				      (:height 30)
				      (:top ,(o-formula (+ (second (gv AGENT-AGG :box)) 10)))
				      (:left ,(o-formula (+ (first (gv AGENT-AGG :box)) 10)))
				      (:line-style ,opal:line-1)
				      (:filling-style ,opal:purple-fill))
				(left-eye ,opal:circle
					  (:width 10)
					  (:height 10)
					  (:top ,(o-formula (+ (second (gv AGENT-AGG :box)) 15)))
					  (:left ,(o-formula (+ (first (gv AGENT-AGG :box)) 15)))
					  (:line-style ,opal:line-1)
					  (:filling-style ,opal:white-fill))
				(right-eye ,opal:circle
					   (:width 10)
					   (:height 10)
					   (:top ,(o-formula (+ (second (gv AGENT-AGG :box)) 15)))
					   (:left ,(o-formula (+ (first (gv AGENT-AGG :box)) 25)))
					   (:line-style ,opal:line-1)
					   (:filling-style ,opal:white-fill))
				(left-pupil ,opal:circle
					    (:width 4)
					    (:height 4)
					    (:top ,(o-formula (+ (second (gv AGENT-AGG :box)) 18)))
					    (:left ,(o-formula (+ (first (gv AGENT-AGG :box)) 18)))
					    (:line-style ,opal:line-1)
					    (:filling-style ,opal:black-fill))
				(right-pupil ,opal:circle
					     (:width 4)
					     (:height 4)
					     (:top ,(o-formula (+ (second (gv AGENT-AGG :box)) 18)))
					     (:left ,(o-formula (+ (first (gv AGENT-AGG :box)) 28)))
					     (:line-style ,opal:line-1)
					     (:filling-style ,opal:black-fill))
				(mouth ,opal:arc
				       (:width 20)
				       (:height 20)
				       (:top ,(o-formula (+ (second (gv AGENT-AGG :box)) 15)))
				       (:left ,(o-formula (+ (first (gv AGENT-AGG :box)) 15)))
				       (:angle1 ,(* 9 (/ PI 8)))
				       (:angle2 ,(* 6 (/ PI 8)))
				       (:line-style ,opal:line-2)
				       (:filling-style ,opal:no-fill))
				(north ,opal:arrowhead
				       (:length 5)
				       (:from-x ,(o-formula (+ 24 (first (gv AGENT-AGG :box)))))
				       (:from-y ,(o-formula (+ 24 (second (gv AGENT-AGG :box)))))
				       (:head-x ,(o-formula (+ 24 (first (gv AGENT-AGG :box)))))
				       (:head-y ,(o-formula (+ 0 (second (gv AGENT-AGG :box)))))
				       (:filling-style ,(o-formula
							 (if (first (gv AGENT-AGG :dir))
							     opal:red-fill
							   opal:gray-fill)))
				       (:open-p nil)
				       (:line-style ,opal:line-1))
				(west ,opal:arrowhead
				      (:length 5)
				      (:from-x ,(o-formula (+ 24 (first (gv AGENT-AGG :box)))))
				      (:from-y ,(o-formula (+ 24 (second (gv AGENT-AGG :box)))))
				      (:head-x ,(o-formula (+ 0 (first (gv AGENT-AGG :box)))))
				      (:head-y ,(o-formula (+ 24 (second (gv AGENT-AGG :box)))))
				      (:filling-style ,(o-formula
							(if (second (gv AGENT-AGG :dir))
							    opal:red-fill
							  opal:gray-fill)))
				      (:open-p nil)
				      (:line-style ,opal:line-1))
				(south ,opal:arrowhead
				       (:length 5)
				       (:from-x ,(o-formula (+ 24 (first (gv AGENT-AGG :box)))))
				       (:from-y ,(o-formula (+ 24 (second (gv AGENT-AGG :box)))))
				       (:head-x ,(o-formula (+ 24 (first (gv AGENT-AGG :box)))))
				       (:head-y ,(o-formula (+ 48 (second (gv AGENT-AGG :box)))))
				       (:filling-style ,(o-formula
							 (if (third (gv AGENT-AGG :dir))
							     opal:red-fill
							   opal:gray-fill)))
				       (:open-p nil)
				       (:line-style ,opal:line-1))
				(east ,opal:arrowhead
				      (:length 5)
				      (:from-x ,(o-formula (+ 24 (first (gv AGENT-AGG :box)))))
				      (:from-y ,(o-formula (+ 24 (second (gv AGENT-AGG :box)))))
				      (:head-x ,(o-formula (+ 48 (first (gv AGENT-AGG :box)))))
				      (:head-y ,(o-formula (+ 24 (second (gv AGENT-AGG :box)))))
				      (:filling-style ,(o-formula
							(if (fourth (gv AGENT-AGG :dir))
							    opal:red-fill
							  opal:gray-fill)))
				      (:open-p nil)
				      (:line-style ,opal:line-1)) )))
     (opal:add-components TOP-AGG AGENT-AGG)))

(defmacro create-world ()
  `(progn
     (create-instance 'WORLD-AGG opal:aggregadget
		      (:dirts '())
		      (:obstacles '()))
     (opal:add-components TOP-AGG WORLD-AGG)))

(defmacro create-vacuum-objects ()
  `(progn
     (create-instance 'OBSTACLE opal:rectangle
		      (:width 50)
		      (:height 50)
		      (:filling-style opal:gray-fill))
     (create-instance 'UNVISITED opal:rectangle
		      (:width 50)
		      (:height 50)
		      (:filling-style opal:motif-light-blue-fill))
     (create-instance 'EMPTY opal:rectangle
		      (:width 50)
		      (:height 50)
		      (:filling-style nil))
     (create-instance 'BUMPED opal:rectangle
		      (:width 50)
		      (:height 50)
		      (:filling-style opal:black-fill))
     (create-instance 'DIRT opal:rectangle
		      (:width 50)
		      (:height 50)
		      (:filling-style opal:light-gray-fill))))


;;; FUNCTIONS FOR MANIPULATION OF OBJECTS

;;; Obstacle

(defun add-obstacle (pos)
  (s-value WORLD-AGG :obstacles (cons pos (gv WORLD-AGG :obstacles)))
  (print-obstacle pos))

(defun print-obstacle (pos)
  (let ((x (* (first pos) 50))
	(y (- 450 (* (second pos) 50))))
    (opal:add-component TOP-AGG  
			(create-instance NIL OBSTACLE (:left x)(:top y)))
    (opal:update AGENT-WIN)))

(defun add-obstacles (l)
  (mapc #'add-obstacle l))

;;; Unvisited

(defun add-unvisited (pos)
  (let ((x (* (first pos) 50))
	(y (- 450 (* (second pos) 50))))
    (opal:add-component TOP-AGG  
			(create-instance NIL UNVISITED (:left x)(:top y)))
    (opal:update AGENT-WIN)))

(defun add-unvisiteds (size obstacles dirts)
  (mapc #'add-unvisited (make-unvisiteds-list-2d (1- (first size))(1- (second size))
						 obstacles dirts)))

(defun make-unvisiteds-list-2d (x y obstacles dirts)
  (let ((unvisited-coord-list
	 (cond
	  ((= x 0) (make-unvisiteds-list-1d x y))
	  (t (append (make-unvisiteds-list-1d x y)
		     (make-unvisiteds-list-2d (1- x) y obstacles dirts))))))
    (remove-coords obstacles (remove-coords dirts unvisited-coord-list))))

(defun remove-coords (coord-list1 coord-list2)
  (cond
   ((endp coord-list2) '())
   ((member (first coord-list2) coord-list1 :test #'equal)
    (remove-coords coord-list1 (rest coord-list2)))
   (t (cons (first coord-list2)
    (remove-coords coord-list1 (rest coord-list2))))))
	    
(defun make-unvisiteds-list-1d (x y)
  (cond
   ((= y 0) (list (list x y)))
   (t (cons (list x y)
	    (make-unvisiteds-list-1d x (1- y))))))

(defun visited (pos)
  (let* ((x (+ 25 (* (first pos) 50)))
	 (y (- 475 (* (second pos) 50)))
	 (obj (opal:point-to-leaf TOP-AGG x y :type unvisited)))
    (if obj (progn (opal:destroy obj) (add-empty pos)))))

;;; Empty

(defun add-empty (pos)
  (let ((x (* (first pos) 50))
	(y (- 450 (* (second pos) 50))))
    (opal:add-component TOP-AGG  
			(create-instance NIL EMPTY (:left x)(:top y)))
    (opal:update AGENT-WIN)))

;;; Bumped

(defun add-bumped (pos)
  (let ((x (* (first pos) 50))
	(y (- 450 (* (second pos) 50))))
    (opal:add-component TOP-AGG  
			(create-instance NIL BUMPED (:left x)(:top y)))
    (opal:update AGENT-WIN)))

;;; Dirt

(defun add-dirt (pos)
  (s-value WORLD-AGG :dirts (cons pos (gv WORLD-AGG :dirts)))
  (print-dirt pos))

(defun print-dirt (pos)
  (let ((x (* (first pos) 50))
	(y (- 450 (* (second pos) 50))))
    (opal:add-component TOP-AGG  
			(create-instance NIL DIRT (:left x)(:top y)))
    (opal:update AGENT-WIN)))

(defun add-dirts (l)
  (mapc #'add-dirt l))

(defun remove-dirt (pos)
  (s-value WORLD-AGG :dirts (remove pos (gv WORLD-AGG :dirts) :test #'equal))
  (clean pos))

(defun clean (pos)
  (let* ((x (+ 25 (* (first pos) 50)))
	 (y (- 475 (* (second pos) 50)))
	 (obj (opal:point-to-leaf TOP-AGG x y :type dirt)))
    (if obj (progn (opal:destroy obj) (add-empty pos)))))


;;; FUNCTIONS FOR CHANGING THE AGENT'S DIRECTION AND POSITION

;;; Direction

(defun change-dir (new-dir)
  (cond 
   ((eq new-dir 'north) (s-value AGENT-AGG :dir '(t nil nil nil)))
   ((eq new-dir 'west) (s-value AGENT-AGG :dir '(nil t nil nil)))    
   ((eq new-dir 'south) (s-value AGENT-AGG :dir '(nil nil t nil)))
   ((eq new-dir 'east) (s-value AGENT-AGG :dir '(nil nil nil t)))))

(defun agent-dir ()
  (let ((dir (gv AGENT-AGG :dir)))
    (cond
     ((first dir) 'north)
     ((second dir) 'west)
     ((third dir) 'south)
     ((fourth dir) 'east))))

(defun new-dir (act dir)
  (let ((lr (second act)))
    (cond
     ((eq lr 'user::right)
      (cond
       ((eq dir 'north) 'east)
       ((eq dir 'west) 'north)
       ((eq dir 'south) 'west)
       ((eq dir 'east) 'south)))
     ((eq lr 'user::left)
      (cond
       ((eq dir 'north) 'west)
       ((eq dir 'west) 'south)
       ((eq dir 'south) 'east)
       ((eq dir 'east) 'north)))
     (t dir))))

;;; Position

(defun change-pos (new-pos)
  (let ((x (* (first new-pos) 50))
	(y (- 450 (* (second new-pos) 50))))
    (if (not (member new-pos (gv WORLD-AGG :obstacles) :test #'equal))
	(progn
	  (s-value AGENT-AGG :box (list x y 48 48))
	  (visited new-pos))
      (add-bumped new-pos))))

(defun agent-pos ()
  (list (/ (first (gv AGENT-AGG :box)) 50) (- 9 (/ (second (gv AGENT-AGG :box)) 50))))

(defun new-pos (pos dir)
  (cond
   ((eq dir 'north)
    (list (first pos) (1+ (second pos))))
   ((eq dir 'west)
    (list (1- (first pos)) (second pos)))
   ((eq dir 'south)
    (list (first pos) (1- (second pos))))
   ((eq dir 'east)
    (list (1+ (first pos)) (second pos)))))


;;; FUNCTIONS FOR INITIALISING, UPDATING AND DESTROYING THE GUI

(defun init-gui (dirts obstacles size)
  (create-window)
  (create-world)
  (create-vacuum-objects)
  (add-unvisiteds size obstacles dirts)
  (add-dirts dirts)
  (add-obstacles obstacles)
  (create-agent)
  (change-dir 'east)
  (change-pos '(1 1))
  (opal:update AGENT-WIN))

(defun update-gui (percept act)
  (cond
   ((and (listp act) (eq (first act) 'user::turn))
    (change-dir (new-dir act (agent-dir))))
   ((eq act 'user::suck)
    (remove-dirt (agent-pos)))
   ((eq act 'user::forward)
    (change-pos (new-pos (agent-pos) (agent-dir)))))
  (sleep 0.5)
  (opal:update AGENT-WIN))

(defun destroy-gui ()
  (opal:destroy AGENT-WIN))
