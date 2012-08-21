;;; File: grid-env.lisp -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;;; Environments with a 2D Grid layout occupied by objects

;;; This file defines a GRID-ENVIRONMENT, a kind of environment where
;;; there is a rectangular grid of spaces, each potentially containing
;;; objects.  (Notice that the basic-env code makes no mention of
;;; space or objects.)  This is sufficient for Vacuum, Wumpus, and
;;; Shopping worlds. The function GRID-ENVIRONMENT-OBJECTS lists all
;;; objects anywhere in the world.  The function GRID-CONTENTS lists
;;; objects in a particular location (i.e., in one square of the
;;; grid).  However, note that objects can be contained within other
;;; objects, and that GRID-CONTENTS takes an optional argument saying
;;; if you want the contained objects, or just the top-level ones.
;;; There is also a function MAP-GRID-CONTENTS to avoid consing up a
;;; list of contained objects.  The OBJECT-LOC of a contained object
;;; is an offset from the container; not an absolute position.  This
;;; makes it easy to move a container without having to iterate over
;;; its contents.  Use TRUE-LOC to get the absolute location of any
;;; object.  For some environments, there can be several objects in
;;; one square, each with slightly different locations.  E.g., in
;;; square (2 3) there might be one object at (2.1 3.1) and another at
;;; (2.5 2.8) this is accomodated by the code; you never have to
;;; truncate coordinates.


;;;; Data Structures for Grid Environments and various Objects

(defstruct (grid-environment
	    (:print-function print-environment)
	    (:include environment
		      (name "Grid environment")
		      (update-fn #'simple-grid-update-fn)
		      (termination-fn #'everyone-dead?)
		      (display-update-fn #'grid-display-update-fn)))
  (grid (make-array '(10 10)))		; A 2-D array of squares
  (start (@ 1 1))			; Where agents begin
  (objects '() :type list)		; List of objects in the environment
  )

(defstruct (object (:print-function print-an-object))
  "An object is anything that occupies space.  Some objects are alive."
  (name "?")				; Used to print the object on the map
  (alive? nil)                          ; Is the object alive?
  (loc (@ 1 1))				; The square that the object is in
  (bump nil)				; Has the object bumped into something?
  (size 0.5)				; Rough diameter of object in meters
  (color 'black)			; Some objects have a color
  (shape 'rectangle)			; Some objects have a shape
  (sound nil)				; Some objects create a sound
  (contents '())			; Some objects contain others
  (container nil)			; Some objects are contained by another
  (orientation 0)			; The direction the object is facing
  )

(defstruct (obstacle (:include object (name "#"))))

(defstruct (wall (:include obstacle)))

(defstruct (agent-body (:include object (alive? t) (name "A")
				 (shape 'cylinder) (color 'metallic)))
  (legal-actions t)			; List of executable actions, or t 
  (holding nil)                         ; Some have a hand to hold 1 thing
  )

;;;; Updating the environment

(defun simple-grid-update-fn (env)
  "The default change function -- just executes each agent's action."
  (for each agent in (environment-agents env) do
       (setf (object-bump (agent-body agent)) nil) ; dissipate bumps
       (execute-action (agent-body agent) (agent-action agent) env)))

(defun execute-action (agent-body action env)
  "Execute the action by applying the function to its arguments,
  if the action is something that the agent can do."
  (let ((legal (agent-body-legal-actions agent-body)))
    (when (or (eq legal t) (member (op action) legal))
      (apply (op action) agent-body env (args action)))))

(defun everyone-dead? (env)
  "Are all the agents dead?  Makes a good termination function."
  (not (some #'(lambda (agent) (object-alive? (agent-body agent)))
	     (environment-agents env))))

;;;; Printing

(defun grid-display-update-fn (stream env)
  (print-environment-map env stream)
  (ascii-display-update-fn stream env))

(defun print-environment-map (env &optional (stream t) (width 3))
  "Show what is in each location in the environment."
  (print-grid (grid-environment-grid env) :stream stream :width width
	      :key #'(lambda (objects)
		       (format nil "~{~A~}"
			       (mapcar #'object-name objects))))
  (values))

(defun print-an-object (object &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "~A" (object-name object)))

(defun grid-environment-x-size (env)
  (array-dimension (grid-environment-grid env) 0))

(defun grid-environment-y-size (env)
  (array-dimension (grid-environment-grid env) 1))

;;;; Initializing Environments

(defun init-environment (&rest args &key (x-size 10) (y-size x-size)
			       (object-specs nil) (agents nil)
			       (start '(1 1))
			       (env-fn #'make-grid-environment)
			       &allow-other-keys)
  "Build a new environment with all the agents at start,
  and various objects scattered in random locations."
  (let ((env (apply env-fn :allow-other-keys t
		    :grid (make-array (list x-size y-size)
				      :initial-element '())
		    args)))
    ;; Add agents and other objects
    (for each agent in agents do
	 (place-object (agent-body agent) start env))
    (init-objects object-specs env)
    env))

(defun init-objects (object-specs env)
  "Place objects, defined by object-specs, in the environment.  The grammar
  for the object-specs language is as follows:
    object-specs => (object-spec...)
    object-spec  => (where what...)
    where        => EDGE | ALL | loc | integer | (LIST where...)
    what         => type | ([how-many] type arg...)
    how-many     => integer | - integer | probability 
  Examples:
    (edge wall)                  1 wall in every perimeter location
    (1 wumpus)                   1 wumpus somewhere (not start square)
    (2 (apple :color green))     An apple is put in each of 2 random locations
    (all (0.25 dirt))            All free locations have 0.25 chance of dirt
    ((2 3) (8 apple) sign)      Location (2 3) has 8 apples and a sign
    ((2 3) (-8 apple))          (2 3) gets 8+random(8.0)-random(8.0) apples
    ((list (1 2) (1 4)) cashier) These two locations each get a cashier
    (2 smoke fire)               2 random locs each get both smoke and fire"
  (for each where-what in object-specs do
       (init-where-what (car where-what) (cdr where-what) env)))

(defun init-where-what (where what env)
  (cond
   ((eq where 'EDGE)    (let ((x-size (grid-environment-x-size env))
			      (y-size (grid-environment-y-size env)))
			  (for i = 0 to (- x-size 1) do
			       (init-what (@ i 0) what env)
			       (init-what (@ i (- y-size 1)) what env))
			  (for i = 1 to (- y-size 2) do
			       (init-what (@ 0 i) what env)
			       (init-what (@ (- x-size 1) i) what env))))
   ((eq where 'ALL)     (dotimes (x (grid-environment-x-size env))
			  (dotimes (y (grid-environment-y-size env))
			    (when (free-loc? (@ x y) env)
			      (init-what (@ x y) what env)))))
   ((xy-p where)        (init-what where what env))
   ((integerp where)    (for i = 1 to where do
			     (init-what (random-loc env :if #'free-loc?)
					what env)))
   ((starts-with where
		 'LIST) (for each w in (rest where) do
			     (init-objects `((,w ,@what)) env)))
   (t (warn "Unrecognized object-spec ignored: ~A." (cons where what)))))

(defun init-what (loc what-list env)
  "Place the objects specified by WHAT-LIST at the given location."
  (for each what in what-list do
       ;; Coerce WHAT into the form (how-many type args...)
       (setf what (mklist what))
       (when (not (numberp (first what))) (push '1 what))
       (destructuring-bind (how-many type . args) what
	 (let ((n (cond ((< how-many 0) (fuzz (- how-many) 1.0 1))
			((not (integerp how-many))
					; (if (< (random 1.0) how-many) 1 0))
					; random ger negativa tal ibland!
			  (if (< (abs (random 1.0)) how-many) 1 0))
			(t how-many))))
	   (for i = 1 to n do
		(place-object (apply (concat-symbol 'make- type) args)
			      loc env t))))))
    

(defun random-loc (env &key (if #'true) (tries 100))
  "Return a random loc, somewhere in the environment.
  The loc must satisfy the :IF predicate.  If it can't find such a location
  after a number of TRIES, it signals an error."
  (or (for i = 1 to tries do
	   (let ((loc (@ (abs (random (grid-environment-x-size env)))
			 (abs (random (grid-environment-y-size env))))))
	     (when (funcall if loc env) (RETURN loc))))
      (error "Can't find a location.")))

(defun free-loc? (loc env)
  "A location is free if there is no obstacle there and it is not the start."
  (and (not (find-object-if #'obstacle-p loc env))
       (not (equal loc (grid-environment-start env)))))

(defun grid-contents (env loc &optional (contained? nil))
  "Return a list of objects in this location, optionally including
  objects that are contained within containers here."
  (if contained?
      (mappend #'object-and-contents (grid-contents env loc nil))
    (aref (grid-environment-grid env) (floor (xy-x loc)) (floor (xy-y loc)))))

(defsetf grid-contents (env loc &optional contained?) (val)
  #-MCL (declare (ignore contained?))
  `(setf (aref (grid-environment-grid ,env)
	       (floor (xy-x ,loc)) (floor (xy-y ,loc)))
	 ,val))

;;;; Dealing with Objects

(defun move-object-to (object loc env)
  "Move an object to an absolute location and return that location.  However,
  attempting to move into a location with an obstacle fails (returns nil)
  and the object receives a bump."
  (cond ((find-object-if #'obstacle-p loc env)
	 (setf (object-bump object) 'bump)
	 nil)
	(t (remove-object object env)
	   (place-object object loc env)
	   loc)))

(defun place-object (object loc env &optional (initial? t))
  "Put the object in its initial position or a new position in environment."
  ;; Coerce agents into agent-bodies
  (when (agent-p object)
    (pushnew object (environment-agents env))
    (setf object (agent-body object)))
  ;; Place the object
  (setf (object-loc object) loc)
  (pushnew object (grid-contents env loc))
  (when initial?
    (push object (grid-environment-objects env)))
  object)

(defun place-in-container (object container env &optional (offset '(0 0)))
  "Put the object inside the container."
  ;; First, remove it from where it was.
  (remove-object object env) 
  ;; Now place it in its new container
  (setf (object-container object) container)
  (setf (object-loc object) offset)
  (pushnew object (object-contents container)))
    
(defun remove-object (object env)
  "Remove the object from its current location."
  (let ((loc (true-loc object))
	(old-container (object-container object)))
    (deletef object (grid-contents env loc))
    (when old-container
      (deletef object (object-contents old-container))
      (setf (object-container object) nil))))

(defun true-loc (object)
  "The absolute location of an object, even if it is contained in something."
  (if (object-container object)
      (add-locs (object-loc object) (true-loc (object-container object)))
    (object-loc object)))

(defun object-and-contents (object)
  "Return a list of object and everything it contains."
  (cons object (mappend #'object-and-contents (object-contents object))))

(defun find-object-if (predicate loc env)
  "Return an object in this location that satisfies this predicate."
  (find-if predicate (grid-contents env loc)))

(defun find-neighbor-if (predicate loc env)
  "Return an object in a neighboring square that satisfies the predicate."
  (let ((x (xy-x loc))
	(y (xy-y loc)))
    ;; Look in the four neighboring squares
    (or (find-object-if predicate (@ x (+ y 1)) env)
	(find-object-if predicate (@ x (- y 1)) env)
	(find-object-if predicate (@ (+ x 1) y) env)
	(find-object-if predicate (@ (- x 1) y) env))))

(defun find-object-or-neighbor-if (predicate loc env)
  "Return an object either in loc or a neighboring square that satisfies
  the predicate."
  (or (find-object-if predicate loc env)
      (find-neighbor-if predicate loc env)))

(defun near? (loc1 loc2 &optional (tolerance 2))
  "Are the two locations nearby each other?"
  (and (< (abs (- (xy-x loc1) (xy-x loc2))) tolerance)
       (< (abs (- (xy-y loc1) (xy-y loc2))) tolerance)))

;;;; Maintaining Orientation and Manipulating Degrees

(defun add-degrees (degree1 degree2)
  "Add two quantitites in degrees such that 0 <= result < 360"
  (mod (+ degree1 degree2) 360))

(defun add-locs (&rest locations)
  "Coordinate-wise addition of locations: (add-locs '(1 2) '(10 20)) = (11 22)"
  (apply #'mapcar #'+ locations))

(defun subtract-locs (&rest locations)
  "Coordinate-wise subtraction of locations."
  (apply #'mapcar #'- locations))

(defun direction->degrees (direction)
  "Translate a direction like LEFT or RIGHT into degrees like +90 or -90"
  (case direction
    (left    +90)
    (right   -90)
    (around +180)
    (otherwise 0)))

(defun orientation->offset (orientation)
  "Convert an orientation like 90 to an offset like (0 1).
  0 degrees is out along the positive X axis; 90 is up the positive y axis."
  (case orientation
    (  0 '(1 0))
    ( 90 '(0 1))
    (180 '(-1 0))
    (270 '(0 -1))))

(defun location-towards (loc orientation)
  "One square from loc in the given direction."
  (add-locs loc (orientation->offset orientation)))

(defun absolute-loc (agent-body offset)
  "Return an absolute location given an offset from an agent, taking the
  agent's orientation into account.  An offset of (1 2) means 1 square to
  the right and two ahead of the agent, given its present orientation."
  (let ((x (xy-x offset))
	(y (xy-y offset)))
    (add-locs (true-loc agent-body)
	      (case (object-orientation agent-body)
		(  0 (@ y (- x)))
		( 90 offset)
		(180 (@ (- y) x))
		(270 (@ (- x) (- y)))))))

(defun offset-loc (agent-body loc)
  "Return an offset from an agent that corresponds to the absolute loc."
  (let ((x (- (xy-x loc) (xy-x (true-loc agent-body))))
	(y (- (xy-y loc) (xy-y (true-loc agent-body)))))
    (case (object-orientation agent-body)
      (  0 (@ (- y) (+ x)))
      ( 90 (@ x y)) 
      (180 (@ (+ y) (- x)))
      (270 (@ (- x) (- y))))))

;;;; Actions 

(defun speak (agent-body env sound)
  "The agent emits a sound."
  (declare (ignore env))
  (setf (object-sound agent-body) sound))

;(defun turn (agent-body env direction)
;  "The agent changes it orientation by turning (a multiple of 90 degrees)."
;  (declare (ignore env))
;  (setf (object-orientation agent-body)
;	(add-degrees (object-orientation agent-body)
;		     (direction->degrees direction))))

(defun turn (agent-body env direction)
      "The agent changes it orientation by turning (a multiple of 90
      degrees)."
      (declare (ignore env))
        (setf (object-orientation agent-body)
        (add-degrees (object-orientation agent-body)
        (direction->degrees direction)))
        (setf (object-name agent-body)
          (cond ((eq (object-orientation agent-body) 0) ">")
             ((eq (object-orientation agent-body) 90) "^")
             ((eq (object-orientation agent-body) 180) "<")
             ((eq (object-orientation agent-body) 270) "v"))))

(defun forward (agent-body env)
  "Move the object to the location that is one step directly ahead of it."
  (let ((destination (location-towards (true-loc agent-body)
				       (object-orientation agent-body))))
    (move-object-to agent-body destination env)))

(defun grab (agent-body env &optional (at-loc (true-loc agent-body)))
  "Grab an object at the specified location.  Assumes a one-handed agent."
  (flet ((grabable? (obj)
	  (and (not (obstacle-p obj))
	       (not (eq obj agent-body))
	       (near? (true-loc obj) at-loc .1))))
    (let ((object (find-object-if #'grabable? at-loc env)))
      (when (and object
		 (not (agent-body-holding agent-body))
		 (near? (true-loc object) (true-loc agent-body)))
	(setf (agent-body-holding agent-body) object)
	(place-in-container object agent-body env
			    (subtract-locs (true-loc object)
					   (true-loc agent-body)))))))

(defun release (agent-body env &optional (to-loc (true-loc agent-body)))
  "Release an object that is in the hand, putting it at the specified loc."
  (let ((object (agent-body-holding agent-body)))
    (when (and object
	       (near? to-loc (true-loc agent-body))
	       (move-object-to object to-loc env))
      (setf (agent-body-holding agent-body) nil))))

;;;; Creating Agents

(defun make-human-agent (&key (name "H") (body (make-agent-body :name name)))
  "Return an agent that does whatever a human user says it should do."
  (make-agent :name name :body body :program (human-agent-program name)))

(defun human-agent-program (name)
  #'(lambda (percept)
      (format t "~&~A perceives ~A and does: " name percept)
      (read)))
