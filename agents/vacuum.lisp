;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;;; The Vacuum World: Top-Level Functions

(defun run-vacuum (&key (agent (reactive-vacuum-agent 'A))
			(x 8)
			(y x)
			(dirtprobability 0.2)
			(hinderprobability 0)
                        (display 'unchanged) (max-steps 200)
			(env (make-vacuum-world :agents (list agent)
						:x-size x
						:y-size y
						:dirt dirtprobability
						:hinder hinderprobability)))
  "Run an agent in a vacuum world (a random one by default)."
  (run-eval-environment env :max-steps max-steps :display display))

(defun vacuum-trials (&key (n 10) (max-steps 200)
			   (agent-types '(reactive-vacuum-agent
					  random-vacuum-agent)))
  "Run the vacuum world n times for different agents; print average scores."
  (agent-trials #'make-vacuum-world agent-types :n n :max-steps max-steps))


(defun vacuum-hinder-trials (&key (n 10) (max-steps 200)
			   (agent-types '(reactive-vacuum-agent
					  random-vacuum-agent)))
  "Run the vacuum world n times for different agents; print average scores."
  (agent-trials #'make-hinder-vacuum-world agent-types :n n :max-steps max-steps))


;;;; Defining Vacuum Worlds

(defstruct (dirt (:include object (name "*"))))

(defun make-vacuum-world (&key (x-size 8) (y-size x-size) (dirt 0.2)
			       (hinder 0) (display nil)
			       (agents (list (reactive-vacuum-agent 'A))))
  "Generate a random vacuum world.  P is the probability of dirt in a square."
  (init-environment
   :name "Vacuum World"
   :agents agents 
   :object-specs `((edge wall) (all (,hinder wall))
			       (all (,dirt dirt)))
   :x-size x-size :y-size y-size 
   :display display 
   :update-fn #'simple-grid-update-fn
   :performance-fn #'vacuum-performance-fn
   :termination-fn #'everyone-dead?
   :percept-fn #'vacuum-world-percept))


(defun make-hinder-vacuum-world (&key (x-size 8) (y-size x-size) (dirt 0.2)
			       (hinder 0.1) (display nil)
			       (agents (list (reactive-vacuum-agent 'A))))
  "Generate a random vacuum world.  P is the probability of dirt in a square."
  (init-environment
   :name "Vacuum World"
   :agents agents 
   :object-specs `((edge wall) (all (,hinder wall))
			       (all (,dirt dirt)))
   :x-size x-size :y-size y-size 
   :display display 
   :update-fn #'simple-grid-update-fn
   :performance-fn #'vacuum-performance-fn
   :termination-fn #'everyone-dead?
   :percept-fn #'vacuum-world-percept))

;;;; Percepts

(defun vacuum-world-percept (agent env)
  "Percept is a three-element sequence: bump, dirt and home."
  (let ((loc (object-loc (agent-body agent))))
    (list (if (object-bump (agent-body agent)) 'bump)
	  (if (find-object-if #'dirt-p loc env) 'dirt)
	  (if (equal loc (grid-environment-start env)) 'home))))

;;;; Performance and Termination Functions

(defun vacuum-performance-fn (agent env)
  "100 points for each piece of dirt vacuumed up, -1 point for each 
  step taken, and -1000 points if the agent does not return home."
  (- (* 100 (count-if #'dirt-p (object-contents (agent-body agent))))
     (environment-step env)
     (if (equal (object-loc (agent-body agent))
		(grid-environment-start env))
	 0
       1000)))

;;;; Actions

(defun suck (agent-body env)
  (let ((dirt (find-object-if #'dirt-p (object-loc agent-body) env)))
    (when dirt
      (place-in-container dirt agent-body env))))

(defun shut-off (agent-body env)
  (declare (ignore env))
  (setf (object-alive? agent-body) nil))

;;;; Agents

(defun vacuum-agent (name &optional (program (human-agent-program name)))
  (make-agent :name name
	      :program program
	      :body (make-agent-body
                     :name '>
		     :legal-actions '(suck forward turn shut-off))))

(defun random-vacuum-agent (name)
  "A very stupid agent: ignore percept and choose a random action."
  (vacuum-agent
   name
   #'(lambda (percept)
       (declare (ignore percept))
       (random-element '(suck forward (turn right) (turn left) shut-off)))))

(defun reactive-vacuum-agent (name)
  "When you bump, turn randomly; otherwise mostly go forward, but
  occasionaly turn.  Always suck when there is dirt."
  (vacuum-agent
   name
   #'(lambda (percept)
       (destructuring-bind (bump dirt home) percept
	 (cond (dirt 'suck)
	       (home (random-element '(shut-off forward (turn right))))
	       (bump (random-element '((turn right) (turn left))))
	       (t (random-element '(forward forward forward
                                     (turn right) (turn left)))))))))

