;;Both aref and setf are concrete examples of generic programming.
#|The first argument in setf is a special sublanguage of Common Lisp, called generalized reference.|#

;;(make-array size)
;;(make-hash-table)

#|
return multiple value (values 3 7)
receive multiple value (multiple-value-bind (v1 v2 v3 ..) (func) (statement))
|#

#|
Reasons that hash cannot always provide the best performance
1. Virtual memory paging and cache misses
2. Hash collisions
3. Inefficiency with samle tables
  the creation and loopup time required make them less efficient than simple structures
4. Varying speed for operations
|#

#|
Reapons that represent an obj rather than a list
1. write a log o error-prone functions to pull the data out
2. Hard to read in print version
3. LIsts work best when you are dealing with information that never changes once the list is created.
|#

#|
senquence functions
1. length vs. list-length
for searching
2. (find-if #'numberp '(a b 5 d)) > 5
3. (count #\s "mississippi") > 4
4. (position #'numberp '(a b 5 d)) > 2
5. (some #'numberp '(a b 5 d)) > T
6. (every #'numberp '(a b 5 d)) > NIL
for iterating
7. (reduce #'+ '(3 4 6 5 2) :initial-value 0) > 20
8. map vs. mapcar
map works on all sequence types, not just lists.
others
9.  (subseq "america" 2 6)
10. (sort '(5 8 2 4 9 3 6) #'<)
|#

#|
Type checkers
arrayp, characterp, consp, functionp, hash-table-p, listp, stringp, and symbolp.
|#

#|
Reasons for why it's bad:
(defun add (a b)
  (cond ((and (numberp a) (numberp b)) (+ a b))
  ((and (listp a) (listp b)) (append a b))))

1. A single monolithic function for all types
2. Modifications required to accommodate new cases
3. Hard to understand
4. Performance

(defmethod add ((a number) (b number)) (+ a b))
(defmethod add ((a list) (b list)) (append a b))
|#

(defparameter *player-health* nil)
(defparameter *player-agility* nil)
(defparameter *player-strength* nil)

(defparameter *monsters* nil) ;;A Heterogeneous array, which can contain different types of monsters.
(defparameter *monster-builders* nil)
(defparameter *monster-num* 12)

(defun orc-battle ()
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    (princ "You have been killed. Game Over."))
  (when (monsters-dead)
    (princ "Congratulations! You have vanquished all of your foes.")))

(defun game-loop ()
  (unless (or (player-dead) (monsters-dead))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless (monsters-dead)
        (show-monsters)
        (player-attack)))
    (fresh-line)
    (map 'list
         (lambda (m)
           (or (monster-dead m) (monster-attack m)))
         *monsters*)
    (game-loop)))

(defun init-player ()
  (setf *player-agility* 30)
  (setf *player-health* 30)
  (setf *player-strength* 30))

(defun player-dead ()
  (<= *player-health* 0))

(defun show-player ()
  (fresh-line)
  (princ "You are a valiant knight with a health of ")
  (princ *player-health*)
  (princ ", an agility of ")
  (princ *player-agility*)
  (princ ", and a strength of ")
  (princ *player-strength*))

(defun player-attack ()
  (fresh-line)
  (princ "Attack style: [s]tab [d]ouble swing [r]oundhouse:")
  (case (read)
    (s (monster-hit (pick-monster)
                    (+ 2 (randval (ash *player-strength* -1)))))
    (d (let ((x (randval (truncate (/ *player-strength* 6)))))
         (princ "Your double swing has a strength of ")
         (princ x)
         (fresh-line)
         (monster-hit (pick-monster) x)
         (unless (monsters-dead)
           (monster-hit (pick-monster) x))))
    (otherwise (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
                        (unless (monsters-dead)
                          (monster-hit (random-monster) 1))))))

(defun randval (n)
  (1+ (random (max 1 n))))

(defun random-monster ()
  (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
        (random-monster)
        m)))

(defun pick-monster ()
  (fresh-line)
  (princ "Monster #:")
  (let ((x (read)))
    (if (not (and (integerp x) (>= x 1) (<= x *monster-num*)))
        (progn (princ "That is not a valid monster number.")
               (pick-monster))
        (let ((m (aref *monsters* (1- x))))
          (if (monster-dead m)
              (progn (princ "That monster is already dead.")
                     (pick-monster))
              m)))))

(defun init-monsters ()
  (setf *monsters*
        (map 'vector
             (lambda (x)
               (funcall (nth (random (length *monster-builders*))
                             *monster-builders*)))
             (make-array *monster-num*))))

(defun monster-dead (m)
  (<= (monster-health m) 0))

(defun monsters-dead ()
  (every #'monster-dead *monsters*))


(defun show-monsters ()
  (fresh-line)
  (princ "Your foes:")
  (let ((x 0))
    (map 'list
         (lambda (m)
           (fresh-line)
           (princ " ")
           (princ (incf x))
           (if (monster-dead m)
               (princ "***dead***")
               (progn (princ "Health=")
                      (princ (monster-health m))
                      (princ ") ")
                      (monster-show m))))
         *monsters*)))

(defstruct monster (health (randval 10)))

(defmethod monster-hit (m x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      (progn (princ "You killed the ")
             (princ (type-of m))
             (princ "! "))
      (progn (princ "You hit the ")
             (print (type-of m))
             (princ x)
             (princ " health points! "))))

(defmethod monster-show (m)
  (princ "A fierce ")
  (princ (type-of m)))

#|
It's similar to waht you can accomplish in popular languages such as C++, Java by defining a generic class and then creating other, more specialized, classes that ingerit fro mthis generic class
|#

(defstruct (orc (:include monster)) (club-level (randval 8)))
(push #'make-orc *monster-builders*)

(defmethod monster-show ((m orc))
  (princ "A wicked orc with a level ")
  (princ (orc-club-level m))
  (princ "club"))

(defmethod monster-attack ((m orc))
  (let ((x (randval (orc-club-level m))))
    (princ "An orc swings his club at yhou and knocks off ")
    (princ x)
    (princ " of your health points.")
    (decf *player-health*)))

(defstruct (hydra (:include monster)))
(push #'make-hydra *monster-builders*)

(defmethod monster-show ((m hydra))
  (princ "A malicious hydra with ")
  (princ (monster-health m))
  (princ " heads."))

(defmethod monster-hit ((m hydra) x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      (princ "the corpse of the fully decapitated and decapacitated hydra falls to the floor!")
      (progn (princ "You lop off")
             (princ x)
             (princ " of the hydra's heads! "))))

(defmethod monster-attack ((m hydra))
  (let ((x (randval (ash (monster-health m) -1))))
    (princ "A hydra attacks you with")
    (princ x)
    (princ " of its heads! It also grows back one more head! ")
    (incf (monster-health m))
    (decf *player-health* x)))

(defstruct (slime-mold (:include monster)) (sliminess (randval 5)))
(push #'make-slime-mold *monster-builders*)

(defmethod monster-show ((m slime-mold))
  (princ "A slime mold with a sliminess of ")
  (princ (slime-mold-sliminess m)))

(defmethod monster-attack ((m slime-mold))
  (let ((x (randval (slime-mold-sliminess m))))
    (princ "A slime mold wraps around your legs and decreases your agility by")
    (princ x)
    (princ "! ")
    (decf *player-agility* x)
    (when (zerop (random 2))
      (princ "It also squirts in your face, taking away a health point! ")
      (decf *player-health*))))

(defstruct (brigand (:include monster)))
(push #'make-brigand *monster-builders*)

(defmethod monster-attack ((m brigand))
  (let ((x (max *player-health* *player-agility* *player-strength*)))
    (cond ((= x *player-health*)
           (princ "A brigand hits your with his slingshot, taking off 2 health points!")
           (decf *player-health* 2))
          ((= *player-agility* x)
           (princ "A brigand catches your leg with his whip, taking off 2 agility points!")
           (decf *player-agility* 2))
          ((= x *player-strength*)
           (princ "A brigand cuts your arm with his whip, taking off 2 strength points! ")
           (decf *player-strength* 2)))))







