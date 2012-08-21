;:cd C:\Users\Initia7_B\Documents\Documents on bryanphone\HPU\CSCI4801\aima
;(progn (load "aima.lisp") (aima-load 'agents))
;(load "week3AgentExtraCredit.lisp")
;(run-vacuum :agent (my-agent 'Bo) :display t :x 8 :y 8 :dirtprobability 0.25 :hinderprobability 0.25)
;(vacuum-trials :agent-types '(reactive-vacuum-agent random-vacuum-agent my-agent))

(defun look-ahead (facing)
	;(prin1 (nth facing *direction*))
	(case (nth facing *direction*)
		(:east (aref memory (+ ax 1) ay))
		(:north (aref memory ax (+ ay 1)))
		(:west  (aref memory (- ax 1) ay))
		(:south (aref memory ax (- ay 1)))))
		
(defun look-right (facing)
	(case (nth facing *direction*)
		(:east (aref memory ax (- ay 1)))
		(:north (aref memory (+ ax 1) ay))
		(:west (aref memory ax (+ ay 1)))
		(:south  (aref memory (- ax 1) ay))))
		
(defun update-coords (facing)
	(format t "old memory ax ay = ( ~a, ~a, ~a ) and facing ~a" ax ay (aref memory ax ay) facing)(print "")
	(case (nth facing *direction*)
		(:east (setq ax (+ ax 1)))
		(:north (setq ay (+ ay 1)))
		(:west  (setq ax (- ax 1)))
		(:south (setq ay (- ay 1))))
	(format t "new memory ax ay = ( ~a, ~a, ~a ) and facing ~a" ax ay (aref memory ax ay) facing)(print "")
	)
		
;(defun go-home ()
;	;(print "GO HOME!")
;	(if (and (= ax 1) (= ay 1)) 'shut-off
;		(cond 
;			; if not on first column, turn if not facing west, then go west
;			((> ax 1) (if (not (= facing 2)) (turn-left) (step-forward)))
;			; if not on first row, turn if not facing south, then go south
;			((> ay 1) (if (not (= facing 3)) (turn-left) (step-forward))))))
		
(defun turn-left ()
	(setq facing (mod (+ facing 1) 4)) ; update internal state
	;(append safe-way-home '(:left))
	'(turn left))
	
(defun turn-right ()
	(setq facing (mod (- facing 1) 4)) ; update internal state
	;(append safe-way-home '(:right))
	'(turn right))
	
(defun step-forward ()
	(setf (aref memory ax ay) :visited)
	(update-coords facing) ; update internal state
	;(append safe-way-home '(:forward))
	'forward)

(defun my-agent (name) 
	(setq memory (make-array (list 50 50)))
	(setq ax 1)
	(setq ay 1)
	(setq maxAx ax)
	(setq maxAy ay)
	;(setf (aref memory ax ay) :visited)
	(setq facing 0)
	(setq *direction* '(:east :north :west :south))
	(setq memBumpCount 0)
	(setq stage 1)

	(format t "facing ~a" facing)(print "")
	(vacuum-agent
	name
	#'(lambda (percept)
		(cond 
			((= stage 1)
				(destructuring-bind (bump dirt home) percept 
					(cond 
						;1 dirt->suck no matter what to maximise score
						;(dirt 'suck)
						;2 bump(real) is only way to percieve boundries
						(bump (setf (aref memory ax ay) :wall) 
							; move internal state back to previous coord, because it never really moved
							(update-coords (mod (+ facing 2) 4)) 
							(turn-left))
						;3 bump(memory) 4 times to switch to stage 2
						((> memBumpCount 3)
							(setq stage 2))
						((not (eql (look-ahead facing) nil))
							(progn 
								(format t "memBumpCount = ~a" memBumpCount)(print "")
								(setq memBumpCount(+ memBumpCount 1))
								(turn-left)))
						;4 home only if all squares are accounted for
						;(home  ; only shut down at home if at least the two adjacent squares have been explored 
						;	(if (and (aref memory 1 2) (aref memory 2 1)) 
						;		'shut-off
						;		(progn (setq turn-count 0)(step-forward))))
						;5 no conditions: check right, then go forward
						(t 	(setq memBumpCount 0)
							(if (> ax maxAx) (setq maxAx ax)) ; save highest ax and-
							(if (> ay maxAy) (setq maxAy ay)) ; ay "visited" (+1 for wall)
							(format t "maxAx = ~a, maxAy = ~a" maxAx maxAy) (print "")
							(if (and  
								(not (or 
									(and (= ay 1) (= facing 0))
									(and (= ax 1) (= facing 3))))
								(string= (look-right facing) nil))
							(turn-right)
							(step-forward))))))
			((= stage 2)
				(prin1 "IN STAGE 2!") 
				;search for missing boundries within 0<ax<=maxAx+1, 0<ay<=maxAy+1
				;   -begin with x = ax, y = ay and spiral counter-clockwise to a look for closest hole
				;	-full spiral range is between maxAx+1 and (maxAx+1)/2 focus at ax (same for y)
				;	-exclude (aref memory maxAx+1 maxAy+1) because we'll never see the top corner
				(let ((x ax)(y ay))	
					(dotimes (xCnt (+ maxAx 1))
						(dotimes (yCnt (+ xCnt 1))
							(while (<= x (+ maxAx 1))
								;ITERATIVE DEEPENING!
								(if (and (null (aref memory x y)) (not (and (= x ax) (= y ay))))
									; check if null because x y is hidden like the middle of a cross
									(format t "Memory missing at ( ~a, ~a )" x y))
								(setq x (+ x 1))))))
				;get back to stage 1
				;search for missing tiles within boundries
				)))))

;stage 1
;	dirt
;		suck
;	bump
;		back up
;		turn left
;	home
;		shut off;		
;	default
;		check right side
;		go forward
;stage 2
;	search for missing boundries (assuming that 0 row/col are boundries).
;		go to missing boundries
;		stage 1
; 	search for missing tiles within boundries
;	go home