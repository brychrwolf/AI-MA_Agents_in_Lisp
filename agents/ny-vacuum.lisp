(defun smart-vacuum-agent (name)
  "Recieves a percept, updates the inner state, chooses an action, 
   updates the inner state, and returns the action"
  (vacuum-agent
   name
   (let ((inner-state (initial-state)) (action))
   #'(lambda (percept)  
       (setq inner-state (update-state-with-percept inner-state percept))
       (setq action (choose-action inner-state))
       (setq inner-state (update-state-with-action inner-state action))
       action))))

(defun initial-state ()
  "Return the initial inner state of a smart vacuum-agent"
  )
	 
(defun update-state-with-percept (state percept)
  "Return a new state based on an old state and a percept"
  )

(defun choose-action (state)
  "Returns an action based on the current state
   Possible actions are: suck, forward, shut-off, (turn left), 
                         (turn right)"
  (random-element '(forward (turn right) (turn left)))
  )

(defun update-state-with-action (state action)
  "Return a new state based on an old state and the action the
   agent is about to perform"
  )

(defun test-agent ()
  (run-vacuum :agent (smart-vacuum-agent 'Bo)
	      :dirtprobability 0.2
	      :hinderprobability 0.2
	      :display nil
	      :gui t
	      :max-steps 10))
