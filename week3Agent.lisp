(defun look-ahead (facing)
	(cond
		((string= (nth facing *direction*) "east")  (aref memory (+ ax 1) ay))
		((string= (nth facing *direction*) "north") (aref memory ax (+ ay 1)))
		((string= (nth facing *direction*) "west")  (aref memory (- ax 1) ay))
		((string= (nth facing *direction*) "south") (aref memory ax (- ay 1)))))
		
(defun update-coords (facing)
	;(format t "old memory ax ay = ( ~a, ~a, ~a )" ax ay (aref memory ax ay))(print "")
	(cond
		((string= (nth facing *direction*) "east")  (setq ax (+ ax 1)))
		((string= (nth facing *direction*) "north") (setq ay (+ ay 1)))
		((string= (nth facing *direction*) "west")  (setq ax (- ax 1)))
		((string= (nth facing *direction*) "south") (setq ay (- ay 1))))
	;(format t "new memory ax ay = ( ~a, ~a, ~a )" ax ay (aref memory ax ay))(print "")
	)
			
(defun step-forward ()
	;(format t "facing = ~a" facing)(print "")
	;(format t "(look-ahead facing) = ~a" (look-ahead facing))(print "")
	(update-coords facing)
	'forward)
		
(defun go-home ()
	;(print "GO HOME!")
	(if (and (= ax 1) (= ay 1)) 'shut-off
		(cond 
			; if not on first column, turn if not facing west, then go west
			((> ax 1) (if (not (= facing 2)) (avoid-step) (step-forward)))
			; if not on first row, turn if not facing south, then go south
			((> ay 1) (if (not (= facing 3)) (avoid-step) (step-forward))))))
	
(defun step-bump () 
	(update-coords (mod (+ facing 2) 4)) ; moves state back to previous coord
	(setq facing (mod (+ facing 1) 4))
	(setq turn-count (+ turn-count 1)) ; count consequtive turns
	'(turn left))
	
(defun avoid-step () ; like bump, but only turn
	(setq facing (mod (+ facing 1) 4))
	(setq turn-count (+ turn-count 1)) ; count consequtive turns
	'(turn left))

(defun my-agent (name) 
	(setq memory (make-array (list 50 50)))
	(setq ax 1)
	(setq ay 1)
	(setf (aref memory 1 1) "visited")
	(setq facing 0)
	(setq *direction* '("east" "north" "west" "south"))
	(setq step-count 0)
	(setq turn-count 0)

	(vacuum-agent
	name
	#'(lambda (percept)
		(setq step-count (+ step-count 1))
		(if (> turn-count 3) (go-home) (progn
			(destructuring-bind (bump dirt home) percept 
				(cond 
					(dirt (setf (aref memory ax ay) "cleaned") 'suck)
					(bump (setf (aref memory ax ay) "wall") (step-bump))
					((or (string= (look-ahead facing) "wall") (string= (look-ahead facing) "visited") (string= (look-ahead facing) "cleaned")) (avoid-step))
					(t (progn
						(setq turn-count 0) ; reset turn-count
						(setf (aref memory ax ay) "visited")
						(step-forward))))))))))
					
;x record all bumps in memory
;x discover height bounds
;x discover width bounds
;x spiral inward
;x find fastest route back home
;x record all forwards in memory
;x check if next forward is visited or wall