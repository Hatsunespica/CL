#|
The reason why lisp so often incorrectly destribed as a functional language has to do with the history.

The real purpose behind functional programming is to separate the functional description of what should happen fro mthe mechanics of how it actaully does happen.

When designing a lisp macro, you want to start with the abstarction first. You want to write programs that use the macro long before you write the macro itself. 
The first setp in serious macro construction is always to write use cases of the macro, even though there is not way you can test or use them.
We are not starting by considering the implementation of a language, but rather asking ourselves what we would like to use this language for.
|#

(load "Ch3.lisp")
(defmacro! defunits% (quantity base-unit &rest units)
  `(defmacro ,(symb 'unit-of quantity) (,g!val ,g!un)
     `(* ,,g!val
	 `,(case ,!gun
	     ((,base-unit) 1)
	     ,@(mapcar (lambda (x)
			 `((,(car x)) ,(cadr x)))
		       (group units 2))))))

#|
In lisp by nesting backquotes we can scale up and down a ladder of quotation.
Every backquote we write takes us one step up this ladder: our code is a list that we may or may not evaluate later. But inside the raw list, every comma encountered takes us back down the quotation ladder and actually excutes this code from the appropriate step on the ladder.
|#


(defun defunits-chaining% (u units)
  (let ((spec (find u units :key #'car)))
    (if (null spec)
	(error "Unknown unit ~a" u)
	(let ((chain (cadr spec)))
	  (if (listp chain)
	      (* (car chain)
		 (defunits-chaining%
		     (cadr chain)
		     units))
	      chain)))))

(defmacro! defunits%% (quantity base-unit  &rest units)
  `(defmacro ,(symb 'unit-of- quantity) (,g!val ,g!un)
     `(* ,,g!val
	 ,(case ,g!un
	    ((,base-unit) 1)
	    ,@(mapcar (lambda (x)
			`((,(car x))
			  ,(defunits-chaining%
			       (car x)
			       (cons `(,base-unit 1)
				     (group units 2)))))
		      (group units 2))))))

;;this implementation may contains cycle. like
#|
(defunits time s
  m (1/60 h)
  h (60 m))
|#


(defun defunits-chaining (u units prev)
  (if (member u prev)
      (error "~{ ~a~^ depends on ~"
	     (cons u prev)))
  (let ((spec (find u units :ket #'car)))
    (if (null spec)
	(error "Unknown unit ~a" u)
	(let ((chain (cadr spec)))
	  (if (listp chain)
	      (* (car chain)
		 (defunits-chaining (cadr chain) units (cons u prev)))
	      chain)))))

(defmacro! defunits (quantity base-unit &rest units)
  `(defmacro ,(symb 'unit-of- quantity)
       (,g!val ,g!un)
       `(* ,,g!val
	 ,(case ,g!un
	    ((,base-unit) 1)
	    ,@(mapcar (lambda (x)
		       `((,(car x))
			 ,(defunits-chaining
			      (car x)
			      (cons `(,base-unit 1)
				    (group units 2))
			    nil)))
		      (group units 2))))))


(defun tree-leaves% (tree result)
  (if tree
      (if (listp tree)
	  (cons (tree-leaves% (car tree)
			      result)
		(tree-leaves% (cdr tree)
			      result))
	  result)))

(defun predicate-splitter (orderp splitp)
  (lambda (a b)
    (let ((s (funcall splitp a)))
      (if (eq s (funcall splitp b))
	  (funcall orderp a b)
	  s))))


(defun tree-leaves%% (tree test result)
  (if tree
      (if (listp tree)
	  (cons
	   (tree-leaves%% (car tree) test result)
	   (tree-leaves%% (cdr tree) test result))
	  (if (funcall test tree)
	      (funcall result tree)
	      tree))))


(defmacro tree-leaves (tree test result)
  `(tree-leaves%%
    ,tree
    (lambda (x)
      (declare (ignorable x))
      ,test)
    (lambda (x)
      (declare (ignorable x))
      ,result)))

#|
Notice the variable x is actually used without it appearing to have been defined.
Tha is because there is an implicit lexical variable bound around eavh of the last two expression.
|#

(defmacro! nlet-tail (n letargs &rest body)
  (let ((gs (loop for i in letargs
	       collect (gensym))))
    `(macrolet
	 ((,n ,gs
	    `(progn
	       (psetq ,@(apply #' nconc
				  (mapcar #'list
					  ',(mapcar #'car letargs)
					  (list ,@gs))))
	       (go ,',g!n))))
       (block ,g!b
         (let ,letargs
           (tagbody
              ,g!n (return-from
                    ,g!b (progn ,@body))))))))

(defun nlet-tail-fact (n)
  (nlet-tail fact ((n n) (acc 1))
	     (if (zerop n)
		 acc
		 (fact (- n 1) (* acc n)))))

(defmacro cxr% (x tree)
  (if (null x)
      tree
      `(,(cond
           ((eq 'a (cadr x)) 'car)
           ((eq 'd (cadr x)) 'cdr)
           (t (error "Non A/D symbol")))
         ,(if (= 1 (car x))
              `(cxr% ,(cddr x) ,tree)
              `(cxr% ,(cons (- (car x) 1) (cdr x))
                ,tree)))))

(defvar cxr-inline-thresh 10)

(defmaco! cxr (x tree)
  (if (null x)
      tree
      (let ((op (cond
                  ((eq 'a (cadr x)) 'car)
                  ((eq 'd (cadr x)) 'cdr)
                  (t (error "Non A/D symbol")))))
        (if (and (integerp (car x))
                 (<= 1 (car x) cxr-inline-thresh))
            (if (= 1 (car x))
                `(,op (cxr ,(cddr x) ,tree))
                `(,op (cxr ,(cons (- (car x) 1) (cdr x))
                           ,tree)))
            `(nlet-tail
              ,g!name ((,g!count ,(car x))
                       (,g!val (cxr ,(cddr x) ,tree)))
              (if (>= 0 ,g!count)
                  ,g!val
                  ;; Will be a tail:
                  (,g!name (- g,!count 1)
                           (,op ,g!val))))))))

(defmacro def-english-list-accessors (start end)
  (if (not (<= 1 start end))
      (error "Bad start./end range"))
  `(progn
     ,@(loop for i from start to end collect
            '(defun
              ,(symb (map 'string
                          (lambda (c) (if (alpha-char-p c) (char-upcase c) #\-))
                          (fromat nil "~:r" i)))
              (arg)
              (cxr (1 a ,(- i 1) d) arg)))))

(defun cxr-symbol-p (s)
  (if (symbolp s)
      (let ((chars (coerce
                    (symbol-name s)
                    'list)))
        (and
         (< 6 (length chars))
         (char= #\C (car chars))
         (char= #\R (car (last chars)))
         (null (remove-if (lambda (c)
                            (or (char= c #\A)
                                (char= c #\D)))
                          (cdr (butlast chars))))))))

(defun cxr-symbol-to-cxr-list (s)
  (labels ((collect (l)
             (if l
                 (list* 1 (if (char= (car l ) #\A) #\A #\D)
                        (collect (cdr l))))))
    (collect
     (cdr (butlast (coerce (symbol-name s) 'list))))))

(defmacro with-all-cxrs (&rest forms)
  `(labels
       (,@(mapcar
           (lambda (s)
             `(,s (l)
                  (cxr ,(cxr-symbol-to-cxr-list s) l)))
           (remove-duplicates (remove-if-not
                               #'cxr-symbol-p
                               (flatten forms)))))
     ,@forms))

(defmacro! dlambda (&rest ds)
  `(lambda (&rest ,g!args)
     (case (car ,g!args)
       ,@(mapcar (lambda (d)
                   `(,(if (eq t (car d))
                          t
                          (list (car d)))
                      (apply (lambda ,@(cdr d))
                               ,(if (eq t (car d))
                                    g!args
                                    `(cdr ,g!args)))))
                 ds))))

