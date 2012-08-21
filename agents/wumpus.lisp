;;; File: wumpus.lisp -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;;; The Wumpus World: Top Level Functions

(defun run-wumpus (&key (agents (list (stupid-wumpus-agent 'A)))
                        (max-steps 100) (display 'unchanged)
			(env (make-wumpus-world :agents agents)))
  "Run a Wumpus world until the game is over."
  (run-eval-environment env :max-steps max-steps :display display))

;;;; New Structures

(defstruct (gold   (:include object (name "$"))))
(defstruct (pit    (:include object (name "O"))))
(defstruct (arrow  (:include object (name "!"))))
(defstruct (wumpus (:include object (name "W") (alive? t))))

;;;; Defining Wumpus Worlds

(defun make-wumpus-world (&key (x-size 8) (y-size x-size)
			       (agents (list (stupid-wumpus-agent 'A)))
			       (object-specs '((edge wall) (1 gold)
					       (1 wumpus) (2 pit))))
  "Build a Wumpus World"
  (init-environment
   :x-size x-size :y-size y-size :name "Wumpus World"
   :percept-fn #'wumpus-percept-fn
   :update-fn #'wumpus-update-fn
   :termination-fn #'wumpus-world-done?
   :performance-fn #'wumpus-world-score
   :object-specs object-specs :agents agents))

(defstruct (wumpus-percept (:type list))
  "A wumpus world percept is a list of five values:"
  stench breeze glitter bump sound)

(defun wumpus-update-fn (env)
  ;; Sounds dissipate:
  (for each object in (grid-environment-objects env) do
       (setf (object-sound object) nil))
  ;; Do each agent's action
  (simple-grid-update-fn env)
  ;; See if anyone died
  (for each agent in (environment-agents env) do
       (when (find-object-if #'deadly? (object-loc (agent-body agent)) env)
	 (kill (agent-body agent)))))

(defun deadly? (object)
  (or (pit-p object)
      (and (wumpus-p object) (object-alive? object))))

(defun wumpus-world-done? (env)
  "End when some agent climbs out or everyone is dead."
  (or (everyone-dead? env)
      (some #'(lambda (agent)
		(and (equal (object-loc (agent-body agent))
			    (grid-environment-start env))
		     (equal (op (agent-action agent)) 'climb)))
	    (environment-agents env))))

(defun wumpus-world-score (agent env)
  "Score 1000 for getting the gold, with penalty of 10000 if dead
  and penalty of 1 for each step taken."
  (let ((agent-body (agent-body agent)))
    (- (if (some #'gold-p (object-contents agent-body)) 1000 0)
       (if (object-alive? agent-body) 0 10000)
       (environment-step env))))

(defun wumpus-percept-fn (agent env)
  "What does the agent perceive?"
  (let ((loc (object-loc (agent-body agent))))
    (make-wumpus-percept
     :stench (if (find-object-or-neighbor-if #'wumpus-p loc env) 'stench)
     :breeze (if (find-object-or-neighbor-if #'pit-p loc env) 'breeze)
     :glitter (if (find-object-if #'gold-p loc env) 'glitter)
     :bump (if (object-bump (agent-body agent)) 'bump)
     :sound (some #'object-sound (grid-environment-objects env)))))

;;;; Actions

(defun climb (agent-body env)
  "Climb out of the cave."
  (declare (ignore agent-body env))
  ;; Only effect is to end the game; see wumpus-world-done?
  nil)

(defun shoot (agent-body env)
  (let ((arrow (find-if #'arrow-p (object-contents agent-body))))
    (when arrow
      (setf (object-contents agent-body)
	    (delete arrow (object-contents agent-body)))
      (propagate-arrow (object-loc agent-body)
		       (object-orientation agent-body) env))))

(defun propagate-arrow (loc orientation env)
  "An arrow keeps going until it kills something or hits a wall."
  (let ((new-loc (location-towards loc orientation)))
    (cond ((find-object-if #'object-alive? new-loc env)
	   (kill (find-object-if #'object-alive? new-loc env)))
	  ((find-object-if #'obstacle-p new-loc env))
	  (t (propagate-arrow new-loc orientation env)))))

(defun kill (object)
  "Make the object no longer alive."
  (when (object-alive? object)
    (setf (object-alive? object) nil)
    (setf (object-sound object) 'scream)))

;;;; Agents

(defun wumpus-agent (name &optional (program (human-agent-program name)))
  (make-agent :name name :program program
	      :body (make-agent-body
                     :contents (list (make-arrow))
                     :legal-actions '(forward turn shoot grab release climb))))

(defun stupid-wumpus-agent (name)
  (wumpus-agent
   name
   (let ((plan nil)
         (wumpus-alive? t))
     #'(lambda (percept)
         (when (wumpus-percept-sound percept)
           (setf wumpus-alive? nil))
	 (cond ((wumpus-percept-glitter percept) 'grab)
	       ((wumpus-percept-bump percept) 
		(setf plan '((turn right) (turn right) forward))
		(pop plan))
	       (plan
		(pop plan))
               ((or (wumpus-percept-breeze percept)
                    (and wumpus-alive? (wumpus-percept-stench percept)))
                (setf plan (list (random-element '((turn left) (turn right)))
                                 'forward))
                (pop plan))
	       (t (random-element '(forward forward (turn right)
					    (turn left)))))))))
	   



(defun wumpus-trials (&key (n 10) (max-steps 100)
			   (agent-types '(stupid-wumpus-agent)))
  "Run the wumpus world n times for different agents; print average scores."
  (agent-trials #'make-wumpus-world agent-types :n n :max-steps max-steps))
