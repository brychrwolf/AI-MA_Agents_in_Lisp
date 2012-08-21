;:cd C:\Users\Initia7_B\Documents\Documents on bryanphone\HPU\CSCI4801\aima
;:cd C:\Documents and Settings\bwolfford\My Documents\T-Mobile5 My Documents\HPU\CSCI4801\aima
;(load "week8ChessRedux.lisp")
;(search-moves white-pieces real-state 3)
;define utility as summation of material value for simplicity

;instantiate "piece" struct for every piece
(progn ;(progn only so that it can be entered on one line in debugger
(defstruct piece :color :type)
(setq wRk1 (make-piece :color 'white :type 'rook))
(setq wKn1 (make-piece :color 'white :type 'knight))
(setq wBs1 (make-piece :color 'white :type 'bishop))
(setq wQn (make-piece :color 'white :type 'queen))
(setq wKg (make-piece :color 'white :type 'king))
(setq wBs2 (make-piece :color 'white :type 'bishop))
(setq wKn2 (make-piece :color 'white :type 'knight))
(setq wRk2 (make-piece :color 'white :type 'rook))
;(setq wPn1 (make-piece :color 'white :type 'pawn))
;(setq wPn2 (make-piece :color 'white :type 'pawn))
;(setq wPn3 (make-piece :color 'white :type 'pawn))
;(setq wPn4 (make-piece :color 'white :type 'pawn))
;(setq wPn5 (make-piece :color 'white :type 'pawn))
;(setq wPn6 (make-piece :color 'white :type 'pawn))
;(setq wPn7 (make-piece :color 'white :type 'pawn))
;(setq wPn8 (make-piece :color 'white :type 'pawn))
;(setq bPn1 (make-piece :color 'black :type 'pawn))
;(setq bPn2 (make-piece :color 'black :type 'pawn))
;(setq bPn3 (make-piece :color 'black :type 'pawn))
;(setq bPn4 (make-piece :color 'black :type 'pawn))
;(setq bPn5 (make-piece :color 'black :type 'pawn))
;(setq bPn6 (make-piece :color 'black :type 'pawn))
;(setq bPn7 (make-piece :color 'black :type 'pawn))
;(setq bPn8 (make-piece :color 'black :type 'pawn))
(setq bRk1 (make-piece :color 'black :type 'rook))
(setq bKn1 (make-piece :color 'black :type 'knight))
(setq bBs1 (make-piece :color 'black :type 'bishop))
(setq bQn (make-piece :color 'black :type 'queen))
(setq bKg (make-piece :color 'black :type 'king))
(setq bBs2 (make-piece :color 'black :type 'bishop))
(setq bKn2 (make-piece :color 'black :type 'knight))
(setq bRk2 (make-piece :color 'black :type 'rook))
)
 
;king is valued 39 (the sum of all other pieces)
(defun piece-utility (type)
	(case type
		;('pawn 1)
		('knight 3)
		('bishop 3)
		('rook 5)
		('queen 9)
		('king 39)))
		
;build "real-state" array to represent the pieces on the board now
;then update array with pieces in their starting locations
(progn ;(progn only so that it can be entered on one line in debugger
(defvar *MAXWIDTH* 8)
(defvar *MAXHEIGHT* 8)
(defvar *MINALPHA* 0)	;the "at least" value
(defvar *MAXBETA* 78)	;the "at most" value
(setq real-state (make-array (list *MAXWIDTH* *MAXHEIGHT*)))
(setf (aref real-state 0 0) wRk1)
(setf (aref real-state 1 0) wKn1)
(setf (aref real-state 2 0) wBs1)
(setf (aref real-state 3 0) wQn)
(setf (aref real-state 4 0) wKg)
(setf (aref real-state 5 0) wBs2)
(setf (aref real-state 6 0) wKn2)
(setf (aref real-state 7 0) wRk2)
;(setf (aref real-state 0 1) wPn1)
;(setf (aref real-state 1 1) wPn2)
;(setf (aref real-state 2 1) wPn3)
;(setf (aref real-state 3 1) wPn4)
;(setf (aref real-state 4 1) wPn5)
;(setf (aref real-state 5 1) wPn6)
;(setf (aref real-state 6 1) wPn7)
;(setf (aref real-state 7 1) wPn8)
;(setf (aref real-state 0 (- *MAXHEIGHT* 2)) bPn1)
;(setf (aref real-state 1 (- *MAXHEIGHT* 2)) bPn2)
;(setf (aref real-state 2 (- *MAXHEIGHT* 2)) bPn3)
;(setf (aref real-state 3 (- *MAXHEIGHT* 2)) bPn4)
;(setf (aref real-state 4 (- *MAXHEIGHT* 2)) bPn5)
;(setf (aref real-state 5 (- *MAXHEIGHT* 2)) bPn6)
;(setf (aref real-state 6 (- *MAXHEIGHT* 2)) bPn7)
;(setf (aref real-state 7 (- *MAXHEIGHT* 2)) bPn8)
(setf (aref real-state 0 (- *MAXHEIGHT* 1)) bRk1)
(setf (aref real-state 1 (- *MAXHEIGHT* 1)) bKn1)
(setf (aref real-state 2 (- *MAXHEIGHT* 1)) bBs1)
(setf (aref real-state 3 (- *MAXHEIGHT* 1)) bQn)
(setf (aref real-state 4 (- *MAXHEIGHT* 1)) bKg)
(setf (aref real-state 5 (- *MAXHEIGHT* 1)) bBs2)
(setf (aref real-state 6 (- *MAXHEIGHT* 1)) bKn2)
(setf (aref real-state 7 (- *MAXHEIGHT* 1)) bRk2)
)

;copy state data to a new state
(defun copy-states (new-state old-state)
	(dotimes (y *MAXWIDTH*)
		(dotimes (x *MAXHEIGHT*)
			(setf (aref new-state x y) (aref old-state x y)))))

;check legality of moves for a piece from xo, yo to xd, yd destination
;using (cond)s to add checks for obstructing pieces later
(defun referee (active-piece xo yo xd yd)
	(cond 
		((or (< xd 0) (> xd 7) (< yd 0) (> yd 7)) nil) ;legal board moves are between 0 and 7
		((and (= xd xo) (= yd yo)) nil)
		(t (case (piece-type active-piece)
			('king (cond
				((and (<= (abs (- xd xo)) 1) (<= (abs (- yd yo)) 1)) t)
				(t nil)))
			('queen (cond
				((or (and (= (abs (- xd xo)) (abs (- yd yo))))
					 (= xd xo)
					 (= yd yo)) t)
				(t nil)))
			('bishop (cond
				((and (= (abs (- xd xo)) (abs (- yd yo)))) t)
				(t nil)))
			;('pawn (cond
			;	;(or (and (= xd xo (or (= (abs (- yd yo)) 1) ("abs=2 if is white and y=1 or is black and y=6")))) t)
			;	((= xd xo) t)
			;	(t nil)))
			('knight (cond
				((or (and (= (abs (- xo xd)) 1) (= (abs (- yo yd)) 2))
					 (and (= (abs (- xo xd)) 2) (= (abs (- yo yd)) 1))) t)
				(t nil)))
			('rook (cond
				((or (= xd xo) (= yd yo)) t)
				(t nil)))))))

;generate list of legal moves for a particular piece in a certain state
(defun all-legal-moves-from (state xo yo)
	(let (movelist null)
	(dotimes (y *MAXWIDTH*)
		(dotimes (x *MAXHEIGHT*)
			(when (referee (aref state xo yo) xo yo x y)
				(setq movelist (cons (list x y) movelist)))))
	movelist))
		
;sums all the pieces of a certain color (opposite than player)
;better utility is when oppenent score low 
;	(need to add for your score high for even numbered states)
;should later account for other features like king protection as well
(defun state-utility (state players-color)
	(let ((sum 0))
	(dotimes (y *MAXWIDTH*)
		(dotimes (x *MAXHEIGHT*)
			(when (and (aref state x y) (not (eql (piece-color (aref state x y)) players-color)))	;when not the players-color, must first check if piece exists to avoid compilor error
				(setq sum (+ (piece-utility (piece-type (aref state x y))) sum)))))
	sum))

;the (setq) is only there to remind me what is being returned
;direct calls to (max-utility) will do the same thing
(defun alpha-beta-search (current-state players-color ply-depth)
	(setq best-move (max-utility current-state 'white (- *MINALPHA* 1) (+ *MAXBETA* 1) ply-depth))
	best-move)

;find best next move in current state based on max-strategy
(defun max-utility (current-state players-color alpha beta ply-depth)
	(let((best-move (list (+ *MAXBETA* 1) nil nil nil nil nil)) ;best-move is a list of (opponents-utility old-x old-y piece-moved new-x new-y)
		(new-state (make-array (list *MAXWIDTH* *MAXHEIGHT*))) ;new-state is the temp array used in the search iterations
		(utility *MINALPHA*)) ;utiltiy is initially set to the lowest "at least" value possible in the game
	(if (< ply-depth 1) 
		(progn ;if terminal state reached (only checking ply-depth counter for now, must add checkmate later)
			(setq best-move (list (state-utility current-state players-color) 0 0 (aref current-state 0 0) 0 0))) ;return just utility value
		(progn ;else check all moves for all of players pieces from all tiles
			(dotimes (y *MAXWIDTH*)
				(dotimes (x *MAXHEIGHT*)
					(when (and (aref current-state x y) (eql (piece-color (aref current-state x y)) players-color)) ;only pick pieces of players-color
						(format t "all legal moves from x=~a y=~a for a ~a:~%" x y (piece-type (aref current-state x y)))
						(dolist (test-move (all-legal-moves-from current-state x y))
							(format t "x=~a y=~a~%" (nth 0 test-move) (nth 1 test-move))
							;for every possible move
							;	create current-state positions into new-state
							;	modify new-state with test-move
							;		put piece in xd yd, then put nil in xo yo
							;		determine the state-utility of new-state
							(copy-states new-state current-state)
							(setf (aref new-state (nth 0 test-move) (nth 1 test-move)) (aref current-state x y))
							(setf (aref new-state x y) nil)
							(setq utility (max utility (nth 0 (min-utility new-state players-color alpha beta (- ply-depth 1)))))
							(if (>= utility beta)
								(progn ;if utiltiy is >= the current highest "at most" value
									(setq best-move (list utility 0 0 (aref current-state 0 0) 0 0)))
								(progn ;else set alpha
									(setq alpha (max alpha utility))))))))
			best-move))))


;find best next move in current state based on max-strategy
(defun min-utility (current-state players-color alpha beta ply-depth)
	(let((best-move (list (+ *MAXBETA* 1) nil nil nil nil nil)) ;best-move is a list of (opponents-utility old-x old-y piece-moved new-x new-y)
		(new-state (make-array (list *MAXWIDTH* *MAXHEIGHT*))) ;new-state is the temp array used in the search iterations
		(utility *MAXBETA*)) ;utiltiy is initially set to the highest "at most" value possible in the game
	(if (< ply-depth 1)
		(progn ;if terminal state reached (only checking ply-depth counter for now, must add checkmate later)
			(setq best-move (list (state-utility current-state players-color) 0 0 (aref current-state 0 0) 0 0))) ;return just utility value
		(progn ;else check all moves for all of players pieces from all tiles
			(dotimes (y *MAXWIDTH*)
				(dotimes (x *MAXHEIGHT*)
					(when (and (aref current-state x y) (eql (piece-color (aref current-state x y)) players-color)) ;only pick pieces of players-color
						(format t "all legal moves from x=~a y=~a for a ~a:~%" x y (piece-type (aref current-state x y)))
						(dolist (test-move (all-legal-moves-from current-state x y))
							(format t "x=~a y=~a~%" (nth 0 test-move) (nth 1 test-move))
							;for every possible move
							;	create current-state positions into new-state
							;	modify new-state with test-move
							;		put piece in xd yd, then put nil in xo yo
							;		determine the state-utility of new-state
							(copy-states new-state current-state)
							(setf (aref new-state (nth 0 test-move) (nth 1 test-move)) (aref current-state x y))
							(setf (aref new-state x y) nil)
							(setq utility (min utility (nth 0 (max-utility new-state players-color alpha beta (- ply-depth 1)))))
							(if (<= utility alpha)
								(progn ;if utiltiy is >= the current highest "at most" value
									(setq best-move (list utility 0 0 (aref current-state 0 0) 0 0)))
								(progn ;else set alpha
									(setq beta (min beta utility))))))))
			best-move))))