(defparameter *big* 100)
(defparameter *small* 1)

(defun guess-numer ()
  (ash (+ *big* *small*) -1))

(defun bigger ()
  (setf *small* (1+ (guess-numer)))
  (guess-numer))

(defun smaller ()
  (setf *big* (1- (guess-numer)))
  (guess-numer))

(defun start-over ()
  (setf *big* 100)
  (setf *small* 1)
  (guess-numer))

