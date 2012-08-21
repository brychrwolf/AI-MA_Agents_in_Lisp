;:cd C:\Users\Initia7_B\Documents\Documents on bryanphone\HPU\CSCI4801\aima
;(load "week8Chess.lisp")
;(search-moves white-pieces ply0 3)
;should start with a wierd initial state besides regular setup so to show search really works.
;can easily be made to be used with more than just two players/colors!

;instantiate "piece" struct for every piece
;capture pile is somewhere (< x 0)
(defstruct piece :color :type :x :y)
;make real pieces
(progn (setq wRk1 (make-piece :color 'white :type 'rook :x 0 :y 0))
(setq wKn1 (make-piece :color 'white :type 'knight :x 1 :y 0))
(setq wBs1 (make-piece :color 'white :type 'bishop :x 2 :y 0))
(setq wQn (make-piece :color 'white :type 'queen :x 3 :y 0))
(setq wKg (make-piece :color 'white :type 'king :x 4 :y 0))
(setq wBs2 (make-piece :color 'white :type 'bishop :x 5 :y 0))
(setq wKn2 (make-piece :color 'white :type 'knight :x 6 :y 0))
(setq wRk2 (make-piece :color 'white :type 'rook :x 7 :y 0))
;(setq wPn1 (make-piece :color 'white :type 'pawn :x 0 :y 1))
;(setq wPn2 (make-piece :color 'white :type 'pawn :x 1 :y 1))
;(setq wPn3 (make-piece :color 'white :type 'pawn :x 2 :y 1))
;(setq wPn4 (make-piece :color 'white :type 'pawn :x 3 :y 1))
;(setq wPn5 (make-piece :color 'white :type 'pawn :x 4 :y 1))
;(setq wPn6 (make-piece :color 'white :type 'pawn :x 5 :y 1))
;(setq wPn7 (make-piece :color 'white :type 'pawn :x 6 :y 1))
;(setq wPn8 (make-piece :color 'white :type 'pawn :x 7 :y 1))
;(setq bPn1 (make-piece :color 'black :type 'pawn :x 0 :y 6))
;(setq bPn2 (make-piece :color 'black :type 'pawn :x 1 :y 6))
;(setq bPn3 (make-piece :color 'black :type 'pawn :x 2 :y 6))
;(setq bPn4 (make-piece :color 'black :type 'pawn :x 3 :y 6))
;(setq bPn5 (make-piece :color 'black :type 'pawn :x 4 :y 6))
;(setq bPn6 (make-piece :color 'black :type 'pawn :x 5 :y 6))
;(setq bPn7 (make-piece :color 'black :type 'pawn :x 6 :y 6))
;(setq bPn8 (make-piece :color 'black :type 'pawn :x 7 :y 6))
(setq bRk1 (make-piece :color 'black :type 'rook :x 0 :y 7))
(setq bKn1 (make-piece :color 'black :type 'knight :x 1 :y 7))
(setq bBs1 (make-piece :color 'black :type 'bishop :x 2 :y 7))
(setq bQn (make-piece :color 'black :type 'queen :x 3 :y 7))
(setq bKg (make-piece :color 'black :type 'king :x 4 :y 7))
(setq bBs2 (make-piece :color 'black :type 'bishop :x 5 :y 7))
(setq bKn2 (make-piece :color 'black :type 'knight :x 6 :y 7))
(setq bRk2 (make-piece :color 'black :type 'rook :x 7 :y 7))
)
;make imagined pieces
(progn (setq iwRk1 (make-piece :color 'white :type 'rook :x 0 :y 0))
(setq iwKn1 (make-piece :color 'white :type 'knight :x 1 :y 0))
(setq iwBs1 (make-piece :color 'white :type 'bishop :x 2 :y 0))
(setq iwQn (make-piece :color 'white :type 'queen :x 3 :y 0))
(setq iwKg (make-piece :color 'white :type 'king :x 4 :y 0))
(setq iwBs2 (make-piece :color 'white :type 'bishop :x 5 :y 0))
(setq iwKn2 (make-piece :color 'white :type 'knight :x 6 :y 0))
(setq iwRk2 (make-piece :color 'white :type 'rook :x 7 :y 0))
;(setq iwPn1 (make-piece :color 'white :type 'pawn :x 0 :y 1))
;(setq iwPn2 (make-piece :color 'white :type 'pawn :x 1 :y 1))
;(setq iwPn3 (make-piece :color 'white :type 'pawn :x 2 :y 1))
;(setq iwPn4 (make-piece :color 'white :type 'pawn :x 3 :y 1))
;(setq iwPn5 (make-piece :color 'white :type 'pawn :x 4 :y 1))
;(setq iwPn6 (make-piece :color 'white :type 'pawn :x 5 :y 1))
;(setq iwPn7 (make-piece :color 'white :type 'pawn :x 6 :y 1))
;(setq iwPn8 (make-piece :color 'white :type 'pawn :x 7 :y 1))
;(setq ibPn1 (make-piece :color 'black :type 'pawn :x 0 :y 6))
;(setq ibPn2 (make-piece :color 'black :type 'pawn :x 1 :y 6))
;(setq ibPn3 (make-piece :color 'black :type 'pawn :x 2 :y 6))
;(setq ibPn4 (make-piece :color 'black :type 'pawn :x 3 :y 6))
;(setq ibPn5 (make-piece :color 'black :type 'pawn :x 4 :y 6))
;(setq ibPn6 (make-piece :color 'black :type 'pawn :x 5 :y 6))
;(setq ibPn7 (make-piece :color 'black :type 'pawn :x 6 :y 6))
;(setq ibPn8 (make-piece :color 'black :type 'pawn :x 7 :y 6))
(setq ibRk1 (make-piece :color 'black :type 'rook :x 0 :y 7))
(setq ibKn1 (make-piece :color 'black :type 'knight :x 1 :y 7))
(setq ibBs1 (make-piece :color 'black :type 'bishop :x 2 :y 7))
(setq ibQn (make-piece :color 'black :type 'queen :x 3 :y 7))
(setq ibKg (make-piece :color 'black :type 'king :x 4 :y 7))
(setq ibBs2 (make-piece :color 'black :type 'bishop :x 5 :y 7))
(setq ibKn2 (make-piece :color 'black :type 'knight :x 6 :y 7))
(setq ibRk2 (make-piece :color 'black :type 'rook :x 7 :y 7))
)

;define utility as summation of material value for simplicity 
;king is valued 0 because of its "invaluablity" and infinity is more difficult to represent
;(should later account for other features like king protection as well)
(defun piece-utility (type)
	(case type
		;('pawn 1)
		('knight 3)
		('bishop 3)
		('rook 5)
		('queen 9)
		('king 0)))

;need to find a better way to make a list of structs to pass to a funtions!!!!
(progn (setq white-pieces ())
(setq white-pieces (cons wRk1 white-pieces))
(setq white-pieces (cons wKn1 white-pieces))
(setq white-pieces (cons wBs1 white-pieces))
(setq white-pieces (cons wQn white-pieces))
(setq white-pieces (cons wKg white-pieces))
(setq white-pieces (cons wBs2 white-pieces))
(setq white-pieces (cons wKn2 white-pieces))
(setq white-pieces (cons wRk2 white-pieces))
;(setq white-pieces (cons wPn1 white-pieces))
;(setq white-pieces (cons wPn2 white-pieces))
;(setq white-pieces (cons wPn3 white-pieces))
;(setq white-pieces (cons wPn4 white-pieces))
;(setq white-pieces (cons wPn5 white-pieces))
;(setq white-pieces (cons wPn6 white-pieces))
;(setq white-pieces (cons wPn7 white-pieces))
;(setq white-pieces (cons wPn8 white-pieces))
)
(progn (setq black-pieces ())
;(setq black-pieces (cons bPn1 black-pieces))
;(setq black-pieces (cons bPn2 black-pieces))
;(setq black-pieces (cons bPn3 black-pieces))
;(setq black-pieces (cons bPn4 black-pieces))
;(setq black-pieces (cons bPn5 black-pieces))
;(setq black-pieces (cons bPn6 black-pieces))
;(setq black-pieces (cons bPn7 black-pieces))
;(setq black-pieces (cons bPn8 black-pieces))
(setq black-pieces (cons bRk1 black-pieces))
(setq black-pieces (cons bKn1 black-pieces))
(setq black-pieces (cons bBs1 black-pieces))
(setq black-pieces (cons bQn black-pieces))
(setq black-pieces (cons bKg black-pieces))
(setq black-pieces (cons bBs2 black-pieces))
(setq black-pieces (cons bKn2 black-pieces))
(setq black-pieces (cons bRk2 black-pieces))
)
(progn (setq iwhite-pieces ())
(setq white-pieces (cons iwRk1 white-pieces))
(setq white-pieces (cons iwKn1 white-pieces))
(setq white-pieces (cons iwBs1 white-pieces))
(setq white-pieces (cons iwQn white-pieces))
(setq white-pieces (cons iwKg white-pieces))
(setq white-pieces (cons iwBs2 white-pieces))
(setq white-pieces (cons iwKn2 white-pieces))
(setq white-pieces (cons iwRk2 white-pieces))
;(setq white-pieces (cons iwPn1 white-pieces))
;(setq white-pieces (cons iwPn2 white-pieces))
;(setq white-pieces (cons iwPn3 white-pieces))
;(setq white-pieces (cons iwPn4 white-pieces))
;(setq white-pieces (cons iwPn5 white-pieces))
;(setq white-pieces (cons iwPn6 white-pieces))
;(setq white-pieces (cons iwPn7 white-pieces))
;(setq white-pieces (cons iwPn8 white-pieces))
)
(progn (setq iblack-pieces ())
;(setq black-pieces (cons ibPn1 black-pieces))
;(setq black-pieces (cons ibPn2 black-pieces))
;(setq black-pieces (cons ibPn3 black-pieces))
;(setq black-pieces (cons ibPn4 black-pieces))
;(setq black-pieces (cons ibPn5 black-pieces))
;(setq black-pieces (cons ibPn6 black-pieces))
;(setq black-pieces (cons ibPn7 black-pieces))
;(setq black-pieces (cons ibPn8 black-pieces))
(setq black-pieces (cons ibRk1 black-pieces))
(setq black-pieces (cons ibKn1 black-pieces))
(setq black-pieces (cons ibBs1 black-pieces))
(setq black-pieces (cons ibQn black-pieces))
(setq black-pieces (cons ibKg black-pieces))
(setq black-pieces (cons ibBs2 black-pieces))
(setq black-pieces (cons ibKn2 black-pieces))
(setq black-pieces (cons ibRk2 black-pieces))
)

;update a state (real or belief) with piece coords		
(defun update-state (white-pieces black-pieces ply)
	(dolist (elem white-pieces)
		(setf (aref ply (piece-x elem) (piece-y elem)) elem))
	(dolist (elem black-pieces)
		(setf (aref ply (piece-x elem) (piece-y elem)) elem)))

;build ply array to represent the pieces on the board now
;and imaginary pieces on the board for searching with
;then update ply0 with initial state
(setq ply0 (make-array (list 8 8)))
(update-state white-pieces black-pieces ply0)
(update-state iwhite-pieces iblack-pieces iply0)
;(setq player-toggle (list white-pieces black-pieces)) ;used to toggle in the recursive function

;create a function that will copy ply data to a new ply
(defun copy-plies (new-ply old-ply)
	(dotimes (i 8)
		(dotimes (j 8)
			(setf (aref new-ply i j) (aref old-ply i j)))))

;move referee
; check legality of moves for a piece to x,y destination
; using cond to add checking-for-pieces-in-the-way checks later
(defun referee (name xd yd)
	(cond 
		((or (< xd 0) (> xd 7) (< yd 0) (> yd 7)) nil)
		((and (= xd (piece-x name)) (= yd (piece-y name))) nil)
		(t (case (piece-type name)
			('king (cond
				((and (<= (abs (- xd (piece-x name))) 1) (<= (abs (- yd (piece-y name))) 1)) t)
				(t nil)))
			('queen (cond
				((or (and (= (abs (- xd (piece-x name))) (abs (- yd (piece-y name)))))
					 (= xd (piece-x name))
					 (= yd (piece-y name))) t)
				(t nil)))
			('bishop (cond
				((and (= (abs (- xd (piece-x name))) (abs (- yd (piece-y name))))) t)
				(t nil)))
			;('pawn (cond
			;	;(or (and (= xd (piece-x name) (or (= (abs (- yd (piece-y name))) 1) ("abs=2 if is white and y=1 or is black and y=6")))) t)
			;	((= xd (piece-x name)) t)
			;	(t nil)))
			('knight (cond
				((or (and (= (abs (- (piece-x name) xd)) 1) (= (abs (- (piece-y name) yd)) 2))
					 (and (= (abs (- (piece-x name) xd)) 2) (= (abs (- (piece-y name) yd)) 1))) t)
				(t nil)))
			('rook (cond
				((or (= xd (piece-x name)) (= yd (piece-y name))) t)
				(t nil)))))))

;generate list of legal moves for a particular piece
(defun all-legal-moves-for (name)
	(let (movelist null)
	(dotimes (i 8)
		(dotimes (j 8)
			(when (referee name i j)
				(setq movelist (cons (list i j) movelist)))))
	movelist))
		
;sums all the pieces of a certain color (opposite than player)
;ignore if piece (< x 1) because piece is captured
;better utility is when oppenent score low 
;	(need to add for your score high for even numbered plys)
(defun state-utility (ply player-color)
	(let ((sum 0))
	(dotimes (i 8)
		(dotimes (j 8)
			(when (not (eql (aref ply i j) nil))	;when not a nil tile
				(when (not (eql (piece-color (aref ply i j)) player-color))	;when not the players color
					(setq sum (+ (piece-utility (piece-type (aref ply i j))) sum))))))
	sum))

;find best next move in current ply rate based on minimax formula
;incorperate alpha-beta pruning using state-utility
(defun search-moves (players-pieces current-ply plyDepth)
	(let ((best-move (list 1064 nil nil nil)) (new-ply (make-array (list 8 8)))) ;best-move is a list of (opponents-resulting-utility piece-moved new-x new-y)
	(dolist (elem players-pieces)
		(when (>= (piece-x elem) 0) ;don't search for the pieces that have been captured
			(dolist (test-move (all-legal-moves-for elem))
				;for every possible move
				;	create new-ply to equal current-ply
				;	modify new-ply with test-move
				;	determine state-utility of new new-ply
				(copy-plies new-ply current-ply)
				(setf (aref new-ply (piece-x elem) (piece-y elem)) nil)			;set old postion to nil
				(setf (aref new-ply (first test-move) (second test-move)) elem)	;set new position to be piece
				(format t "~a ~a ~a from (~a ~a)~%" test-move (piece-color elem) (piece-type elem) (piece-x elem) (piece-y elem))	; *UNCOMMENT this to see all the possible moves as they are being tested*
				(setq score (state-utility new-ply (piece-color (first players-pieces)))) ;use 'score' for berevity. determines color by asking the first piece from the provided list
				(when (< score (first best-move))  ;picks the first best move found, ignoring newer moves of same utility
					(setq best-move (list score elem (first test-move) (second test-move)))))))
	;now that we know the best-move
	;	reset new-ply 1 more time with copy-plies 
	;	perform best-move on fresh new-ply
	;	update the new-ply's moved-piece's x and y with new value
	;	send new-ply back into search-moves if still looking deeper
	(copy-plies new-ply current-ply)
	(setf (aref new-ply (piece-x (second best-move)) (piece-y (second best-move))) nil)		;set old postion to nil
	(setf (piece-x (aref new-ply (third best-move) (fourth best-move))) -1)					;move piece that is in new postion to (= x -1)
	(setf (aref new-ply (third best-move) (fourth best-move)) (second best-move))			;set new position to be piece
	(setf (piece-x (second best-move)) (third best-move))	;	update the new-ply's moved-piece's 
	(setf (piece-y (second best-move)) (fourth best-move))	;	x and y with new value
	(format t "current-ply = ~a~%" current-ply)	(format t "new-ply = ~a~%" new-ply)	(format t "best-move = ~a~%" best-move) 	; *UNCOMMENT this to see all the plys as they are being tested*
	(when (> plyDepth 1)
		(if (eql (piece-color (first players-pieces)) 'white) ;toggle player color by check for even/odd values of plyDepth
			 (setq best-move (search-moves black-pieces new-ply (- plyDepth 1)))
			 (setq best-move (search-moves white-pieces new-ply (- plyDepth 1)))))
	best-move))