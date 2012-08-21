(defun random-plus-state-vacuum-agent (name)
  "Smart vacuum agent with internal state.
   Recieves a percept, updates the inner state, chooses an action, 
   updates the inner state, and returns the action"
  (vacuum-agent
   name
   (let ((inner-state (initial-state)) (action))
   #'(lambda (percept)  
       (setq inner-state (update-state-with-percept inner-state percept))
       (setq action (choose-action inner-state))
       (setq inner-state (update-state-with-action inner-state action))
       action))))

; Return the initial inner state of a smart vacuum-agent - 

(defun initial-state ()
  0
  )

; Return a new state based on an old state and a percept
(defun update-state-with-percept (state percept)
  state
  )

; Returns an action based on the current state
; Possible actions are: suck, forward, turn-off, (turn left), 
;                       (turn right)
(defun choose-action (state)
  (random-element '(suck forward (turn right) (turn left)))
  )

; record how many times we suck
(defun update-state-with-action (state action)
  (format t " test state: ~a" state)
  (cond ((eql action 'suck) (+ state 1))
        (t state))
  )

(defun test-agent ()
  (run-vacuum :agent (smart-vacuum-agent 'Bo)
	      :dirtprobability 0.2
	      :hinderprobability 0.2
	      :display nil
	      :gui t
	      :max-steps 10))
