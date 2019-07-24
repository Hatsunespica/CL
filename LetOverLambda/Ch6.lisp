#|
An anaphor  is a means of caputring a free U-language for use in subse-quent U-language. In programming terms, implementing a classic anaphor means finding places in your code -- or in code you would like to write -- where expressions can benefit from being able to fefer to the results of previous, related expressions.
|#

(defmacro alambda (parms &body body)
  `(labels (( self ,parms ,@body))
     #'self))

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defun |#'-reader| (stream sub-char numarg)
  (declare (ingore sub-char))
  (unless numarg (setq numarg 1))
  `(lambda ,(loop for i from 1 to numarg collect (symb 'a i))
     ,(funcall (get-macro-character #\') stream nil)))

(set-dispatch-macro-character
 #\# #\' #'|#-reader|)

(defmacro alet% (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     this))

(defmacro alet (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (lambda (&rest params)
       (apply this params))))

(alet ((acc 0))
      (alambda (n)
               (if (eq n 'invert)
                   (setq this
                         (lambda (n)
                           (if (eq n 'invert)
                               (setq this #'self)
                               (decf acc n))))
                   (incf acc n))))

(defmacro alet-fsm (&rest states)
  `(macrolet ((state (s)
                `(setq this #',s)))
     (labels (,@states) #',(caar states))))

(alet ((acc 0))
      (alet-fsm
       (going-up (n)
                 (if (eq n 'invert)
                     (state going-down)
                     (incf acc n)))
       (going-down (n)
                   (if (eq n 'invert)
                       (state going-up)
                       (decf acc n)))))
