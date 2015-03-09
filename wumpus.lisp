;;;; Hunt the Wumpus

;;; How to play
;;; --------------------------------------------------
;;; Load into REPL and start game with start function
;;;
;;; Show rooms you can move into with show-rooms
;;;
;;; Move into a room with move(room name)
;;;
;;; Shoot at the wumpus using shoot - provide a list
;;; with a possible 5 rooms to shoot at (must connect)
;;;
;;; Avoid bats (which move you to random room,  pits
;;; (which cause your death) and the Wumpus room (the
;;; Wumpus will eat you if you enter its room)

(defparameter *current-room* (random 20))
(defparameter *arrows* 5)

(defun create-maze ()
  (defparameter *maze*
	'((1 4 7) (0 2 9) (1 3 11) (2 4 13) (0 3 5)
	  (4 6 14) (5 7 16) (0 6 8) (7 9 17) (1 8 10)
	  (9 11 18) (2 10 12) (11 13 19) (3 12 14) (5 13 15)
	  (14 16 19) (6 15 17) (8 16 18) (10 17 19) (12 15 18))))

(defun place-gotchas ()
  (defparameter *wumpus* (random 20))
  (defparameter *bats* (list (get-random) (get-random)))
  (defparameter *pits* (list (get-random) (get-random))))

(defun start ()
  (format t "restarting game...~%")
  (place-gotchas)
  (create-maze)
  (setf *arrows* 5)
  (setf *current-room* (random 20)))

(defun move(n)
  (if 
   (member n (nth *current-room* *maze*))
   (progn
     (setf *current-room* n)
     (check-room)
     (format t "moved to ~a. doors to: ~a"
	     *current-room* (nth *current-room* *maze*)))
   (format t "available rooms: ~a" (nth *current-room* *maze*))))

(defun check-room ()
  (if (eql *wumpus* *current-room*) (format t "the wumpus got you~%")
      (if (member *current-room* *bats*)
	    (let ((x (get-random)))
	    (format t "bats! move you to room ~a~%" x)
	    (setf *current-room* x)
	    (check-room)
	    (return-from check-room))
	  (if (member *current-room* *pits*) 
	      (progn
		(format t "you fell in a pit~%")
		(start)))))
  (dolist (room (nth *current-room* *maze*)) (check-room-adj room)))

(defun check-room-adj (n)
  (if (eql *wumpus* n) (format t "i smell a wumpus~%"))
  (if (member n *bats*) (format t "bats nearby~%"))
  (if (member n *pits*) (format t "i feel a draft~%")))

(defun get-random ()
  (random 20))

(defun show-rooms()
  (format t "available rooms: ~a" (nth *current-room* *maze*)))

(defun shoot (path)
  (if (< (length path) 5)
      (progn
	(dotimes (i (length path))
	  ;;Check for wumpus
	  (if (eql (nth i path) *wumpus*) 
	      (progn
		(format t "You shot the wumpus!~%")
		(start)
		(return-from shoot)))
	  ;;Check if room connects
	  (if (eql i (- (length path) 1)) (return-from shoot))
	  (if (member (nth (+ i 1) path) (nth (nth i path) *maze*))
	      (format t "can traverse path~%")
	      (return-from shoot))
	  (- *arrows* 1))
	(setf *wumpus* (random 20)))
      (format t "Can't shoot further than 5")))

