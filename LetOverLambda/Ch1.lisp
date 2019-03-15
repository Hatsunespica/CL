;; the primary use of macros is not to improve the efficency of code at run-time
;; but make the job of programming a desired application easier.
(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest arfgs)
  (values (intern (apply #'mkstr args))))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
	     (let ((rest (nthcdr n source)))
	       (if (consp rest)
		   (rec rest (cons
			      (subseq source 0 n)
			      acc))
		   (nreverse
		    (cons source acc))))))
    (if source (rec source nil) nil)))

(defun flatten (x)
  (labels ((rec (x acc)
	     (cond ((null x) acc)
		   ((atom x) (cons x acc))
		   (t (rec
		       (car x)
		       (rec (cdr x) acc))))))
    (rec x nil)))

(defun fact (x)
  (if (= x 0)
      1
      (* x (fact (- x 1)))))

(defun choose (n r)
  (/ (fact n)
     (fact (- n r))
     (fact r)))
