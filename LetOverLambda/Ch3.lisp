;; The construction of a macro is an iterative process: all complex macros come from simpler ones.

;; sleep : nat -> void, receives a non-negative, non-cimplex, numeric argument, and pause execution of the process for n seconds.

;;eq is the fastest lisp comparison operator and roughly correspionds to a pointer comparison. A convenient way to let two or more different lisp expressions know you're referring to the same thing.

;;NUmbers and strings and some other primitives evaluate to themselves which is why we don;t need to quote the numeric values given to sleep-units%.

;;Symbols, however, don't typucally evaluate to themselves. When lisp evaluates a symbol it assumes you are referring to a variable and tries to look up the value associated wieht that variable given your lexical context.
;;As a rule, no rule without exception. Some symbols do evaluate to thenseleves, for example: t, nil and keywords.


;; we assume that we already know the the unit, rather than inputed from outside. Otherwise it's better to use a defun rather than defmacro.
(defmacro sleep-units (value unit)
  `(sleep
    (* ,value
     (case ,unit
	((s) 1)
	((m) 60)
	((h) 3600)
	((d) 84600)
	((ms) 1/1000)
	((us) 1/1000000)))))

(defmacro unit-of-time (value unit)
  `(* ,value
      ,(case unit-of-time
	 ((s) 1)
	 ((m) 60)
	 ((h) 3600)
	 ((d) 86400)
	 ((ms) 1/1000)
	 ((us) 1/1000000))))

;;Control Structures
(defmacro nlet (n letargs &rest body)
  `(labels ((,n ,(mapcar #'car letargs)
	      ,@body))
     (,n ,@(mapcar #'cadr letargs))))

;; In all kinds of lisp programming, including macro construction, we splice, merge, reduce, map, and filter lists. The only difference is that when programming macros, the output subsequently gets passed to a compiler or interpreter.

;; When people say "only use macros when functions won't do", they mean that for any definition where you don't want to evaluate certain arguments, or you want to evaluate them out of order, or more than once, you will need to use a macro. Functions, no matter how cleverly written, simply will not work.

;;freedom variables
;; But freedom doesn't refer to what the expression can do, rather what, we can take the expression and embed it anywhere we want, allowing our expression to access a binding in the surounding code.

;;inject free variables into codes.

;;One popular solution to solve not referentially transparentis hygienic macros. However, the author thinks it's helpful to beginners, and. in the worst case, limits  their users. Furthermore, there are still some cases hygienic macros can be vulnerable.

;; Another solution is generated symbols. Lisp generates a name that would never has the same.
;; CL separates the variable namespace from the function namespace and eliminates an entire dimension of unwanted variable capture problems.

(defmacro nif (expr pos zero neg)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ((plusp ,g) ,pos)
	     ((zerop ,g) ,zero)
	     (t ,neg)))))

