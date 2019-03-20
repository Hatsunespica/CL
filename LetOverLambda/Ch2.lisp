;; Object can be built from little more than assignable value cells and good old lambda expressions.

;;closures represent a bettwe abstraction to use when constructing macros.

;;lisp always assumes you mean to use indefinite extent.

;;Variables don't have types, only values have types..
;; cons cell can store pointers.
;;Lexical scopes are so related to closures that they often referred as lexical closures.
;;(defvar tmp), tmp is an unbound variable and has no value. However, (let ((x)) x) and x has a default value even if you don't assign a value to it explicitly.
;;A special variable actually always does refer to the same location in memory. when using let to bind a special variable, a copy of the variable would be done, and over-write the memory location with new value, and at last, restore the orginal value from the copy.

;; (lambda (x) x) 1) <-> (let ((x 1)) x)
;; Sometimes data structure that bundle together code and data are called objects.
;; An object bis a collection of procedures and some associated state.
;; A closure is like an object taht has exactly one method: funcall.
(let ((counter 0))
  (lambda () (incf counter)))

;; Although closures are always a single function and its enclosing environment, the multiple methods, inner classes, and static variables of object systems all have their closure counterparts.

;; multiple methods.
(let ((counter 0))
  (values
   (lambda () (incf counter))
   (lambda () (decf counter))))

;; object, collections of procedures with associated stat.
;; classes, the data structure used to create objects.

;; classes
(lambda ()
  (let ((counter 0))
    (lambda () (incf counter))))

;; defun supplies an implicit lambda
(defun counter-class ()
  (let ((counter 0))
    (lambda () (incf counter))))

;; block-scanner
(defun block-scanner (trigger-string)
  (let* ((trig (coerce trigger-string 'list))
	 (curr trig))
    (lambda (data-string)
      (let ((data (coerce data-string 'list))
	    (curr trig))
	(dolist (c data)
	  (if curr
	      (setq curr
		    (if (char= c (car curr))
			(cdr curr)
			trig))))
	(not curr)))))
;; (defcar scanner (block-scanner "foo"))
;; In fact, the scanner is an object like the object we get in C++ or Java.
;; (funcall scanner "bar")
;; and like this, we use  funcall to call the method.

;; an obvious deduction is that we can implement class variable or static variable, namely variable shared between objects.

(let ((direction 'up))
  (defun toggle-counter-direction ()
    (setq direction
	  (if (eq direction 'up)
	      'down
	      'up)))
  (defun counter-class ()
    (let ((counter 0))
      (lambda ()
	(if (eq direction 'up)
	    (incf counter)
	    (decf counter))))))

;; Object systems are a formalisation of a subset of let and lambda combinations
;; the object need not be a primitive notion in programming languages.
;; Once assignable value cells and good old lambda expressions are available, object systems are, at best, occasionally useful abstractions and, at worst, special-case and redundant.
