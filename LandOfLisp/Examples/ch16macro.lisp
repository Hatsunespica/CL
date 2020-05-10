#|
(defun add (a b)
  (let ((x (+ a b)))
    (format t "The sum is ~a" x)
    x))
Even though we only a single x here, we still need a lot of parentheses. This is one kind of visual noise.
|#

(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))

#|
Macro expansion runs at a different time than a function is run.The job of macro expansion is to find any macros in the code and to convert them into regular Lisp code. This is called macro expansion time
|#

#|
Be care with once-only
(defmacro split (val yes no)
  `(if ,val
  (let ((head (car ,val))
    (tail (cdr ,val)))
    ,yes)
  ,no))
By the way, macros that automatically generate variables like this one are called anaphoric macros.
|#

#|
Be careful with referential transparency.
(defmacro split (val yes no)
  `(let1 x ,val
    (if x
      (let ((head (car x))
            (tail (cdr x)))
        ,yes)
      ,no)))
This macro has an uncessary variable captuing in following example.
(let1 x 100
    (split '(2 3)
      (+ x head)
      nil))
|#

(defmacro split (val yes no)
  (let1 tmp (gensym)
    `(let1 ,tmp ,val
       (if ,tmp
           (let ((head (car ,tmp))
                 (tail (cdr ,tmp)))
             ,yes)
           ,no))))

(defun pairs (lst)
  (labels ((f (lst acc)
             (split lst
                    (if tail
                        (f (cdr tail) (cons (cons head (car tail)) acc))
                        (reverse acc))
                    (reverse acc))))
    (f lst nil)))

(defmacro recurse (vars &body body)
  (let1 args (pairs vars)
    `(labels ((self ,(mapcar #'car args)
                ,@body))
       (self ,@(mapcar #'cdr args)))))

(defun my-length (lst)
  (recurse (lst lst acc 0)
           (split lst
                  (self tail (1+ acc))
                  acc)))


#|
Drawbacks:
One main drawbacks is hard for others to read and understand, which requires a detailed documentation for better utilization. Also, other alternatices could be considered.
|#

(defun my-length1 (lst)
  (reduce (lambda (x i)(1+ x))
          lst :initial-value 0))

