;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;;; The basic environment simulator code.

;;; This file defines the environment simulator function: RUN-EVAL-ENVIRONMENT.
;;; It is a little different from the psudo-code in the book: the update and
;;; termination functions and the agents are folded into the environment, to
;;; make it easier to handle them as a whole; there is a max-steps parameter to
;;; keep the environment from getting into an infinite loop; there is a
;;; parameter to control whether intermediate results are displayed, and a
;;; separate function, run-eval-environment-1-step that does a single time step
;;; (so that we can call this function separately).

;;; This file also defines four basic data types: environment, agent, action
;;; and percept.  Actions and percepts can be pretty much anything, but an
;;; ENVIRONMENT must have a percept-fn (that gives an agent its percept), an
;;; update-fn (that takes the agents' actions and any other facts about the
;;; environment and updates it), a termination-fn (that says when to stop), and
;;; a performance-fn (that updates the score of an agent).  An environment also
;;; holds a list of agents, a count of how many steps have been executed, a
;;; name, and some functions for displaying the progress of the simulation on
;;; the stream held in the display slot.  (If this slot is nil, nothing is
;;; displayed).  An AGENT consists of an agent program (that maps from percepts
;;; to actions), a name and current score, and the current percept and action.
;;; The body slot can be used to hold a manifestation of the agent's physical
;;; properties, if desired.

;;; To use this code, define an environment and call RUN-ENVIRONMENT.
;;; Currently this is the same as RUN-EVAL-ENVIRONMENT.  Or call AGENT-TRIALS
;;; to compare the performance of several different agents in a set of similar
;;; environments.

;;;; Data Types

(defstruct (environment (:print-function print-environment))
  (percept-fn (required))		; fn: agent x environment -> percept
  (update-fn (required))		; fn: environment -> environment
  (termination-fn #'false)		; fn: environment -> boolean
  (performance-fn #'count-actions)	; fn: agent x environment -> number
  (agents '() :type list)
  (state nil)
  (step 0 :type integer)
  (name "Generic environment")
  (display nil)
  (display-initial-fn #'nothing)
  (display-update-fn  'ascii-display-update-fn)
  (display-final-fn   #'nothing))

(defstruct (agent (:print-function print-agent))
  (program (required))			; fn: percept -> action
  (body nil)
  (name "A")
  (score 0)
  (percept nil :type percept)
  (action nil :type action))

(deftype action () '(or symbol list))	; E.g. FORWARD or (TURN RIGHT)

(deftype percept () 't)			; A percept can be anything

;;;; Main Functions

(defun run-environment (&rest args)
  "Basic environment simulator; the same as run-eval-environment. [p 48]"
  (apply #'run-eval-environment args))

(defun run-eval-environment (env &key (max-steps 1e6) (display 'unchanged) (gui nil))
  "Basic environment simulator.  It gives each agent its percept, gets an
  action from each agent, and updates the environment. [p 48]"
  (unless (eq display 'unchanged)
    (setf (environment-display env) display))
  (display-env env 'environment-display-initial-fn env)
  (repeat (run-eval-environment-1-step env gui)
	  until (or (>= (environment-step env) max-steps)
		    (funcall (environment-termination-fn env) env)))
  (display-env env 'environment-display-final-fn env)
  env)

(defun agent-trials (environment-fn agent-types &key (n 10) (max-steps 1000))
  "Report how well a single agent does in a set of N similar environments,
  and compare that to other agents in the same set of environments.
  Environment-fn takes a :agents keyword argument, and returns an environment.
  agent-types is a list of names of functions that each create an agent."
  (let ((random-state (make-random-state t)))
    (with-collection ()
     (for each agent-type in agent-types do
	  (let ((score (agent-trial environment-fn agent-type
				    (make-random-state random-state)
				    :max-steps max-steps :n n)))
	    (collect score)
	    (format t "~&~10,2F average for ~A" score agent-type))))))

;;;; Auxiliary Functions

(defun agent-trial (environment-fn agent-type *random-state*
				   &key (n 10) (max-steps 1000))
  "Run n environments with an identical agent in each, and average the scores."
  ;; By binding *random-state*, we hope to reproduce the same set of
  ;; environments each time AGENT-TRIAL is called with the same environment-fn.
  (/ (with-collection (sum 0 +)
      (for i = 1 to n do
	   (let* ((agent (funcall agent-type agent-type))
		  (env (funcall environment-fn :agents (list agent))))
	     (run-eval-environment env :max-steps max-steps)
	     (sum (agent-score agent)))))
     (float n)))

(defun run-eval-environment-1-step (env gui)
  "Run the environment for one time step -- each agent does its thing."
  (incf (environment-step env))
  (for each agent in (environment-agents env) do
       (setf (agent-percept agent) (get-percept agent env))
       (setf (agent-action agent) (run-agent-program agent))
       (if gui (agent-gui:update-gui (agent-percept agent) (agent-action agent))))
  (funcall (environment-update-fn env) env)
  (for each agent in (environment-agents env) do
       (setf (agent-score agent)
	     (funcall (environment-performance-fn env) agent env)))
  (display-env env 'environment-display-update-fn env)
  env)

(defun get-percept (agent env)
  "Use the env's percept function to determine what percept the agent gets."
  (funcall (environment-percept-fn env) agent env))

(defun run-agent-program (agent)
  "Run the agent's program to determine what action it takes."
  (funcall (agent-program agent) (agent-percept agent)))

(defun count-actions (agent env)
  "The default performance function: counts the number of actions,
  subtracting 1 point for each one."
  (declare (ignore agent))
  (- (environment-step env)))

;;;; Printing and Displaying the progress of the simulation

(defun print-environment (env &optional (stream t) depth)
  (declare (ignore depth))
  (print-unreadable-object (env stream)
    (format stream "~A; Step: ~D, Agents:~{ ~A~}"
	    (environment-name env) (environment-step env)
	    (environment-agents env))))

(defun print-agent (agent &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "~A=~D" (agent-name agent) (agent-score agent)))

(defun display-env (env fn &rest args)
  "Call a display function if the environment has a stream to display on."
  (let ((stream (environment-display env)))
    (when stream
      (apply (funcall fn env) stream args))))

(defun ascii-display-update-fn (stream env)
  (format t "~&Time step ~D:~%" (environment-step env))
  (for each agent in (environment-agents env) do
       (format stream "~&Agent ~A perceives ~A~%~6Tand does ~A~%"
               agent (agent-percept agent) (agent-action agent))))



