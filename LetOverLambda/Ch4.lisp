#|
a special type of macro, called read macro, opertes on the raw characters that make up your program.
The read macro is the device we use to process non-lisp syntax beforethe lisp reader get its paws on it.
Lisp gives you hooks for controlling every aspect of its behaviour. In other words, lisp lets you extend the reader, so that non-lisp objects are actually read in as lisp objects. Just as you build your application on top of lisp, extending it with macros and functions , lisp applications can, and frequently do, ooze out into this dimension of extensibility as well.
#. lets forms evaluated at read time, not when the form is evaluated.
* variable contains the value that resulted from evaluating the previous form, 
and the + variable contains that form.
because evaluation is finished after read,  (equal * (eval +)) is always T. However, if we we backquote and comma, this doesn't happen.

,@ can be used to insert elements into the middle of a list.
,@ is frobidden to modify the lists being spliced in.
CL also provides a destructive version of splicing unquote which can be used anywhere splicing unquote can, which is ,. .
Maybe there are some cases scaring, but there are still many operations that create fresh list, structure that yo uare probably planning on throwing out anyways. 
e.g. splicing the results of a mapcar is common and safe.
(defun safer-use-of-bq ()
  `(a
    ,. (mapcar #'fun '(b c d))
    e))

Backquote is a read macro. 
quines: writing an expression that evalulates to itself.
(let ((let '`(let ((let ',let))
	       ,let)))
  `(let ((let ',let)) ,let))
|#

;; reading strings
;; customise delimiters

(defun |#"-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let (chars)
    (do ((prev (read-char stream) curr)
	 (curr (read-char stream) (read-char stream)))
	((and (char= prev #\") (char= curr #\#)))
      (push prev chars))
    (coerce (nreverse chars) 'string)))

(set-dispatch-macro-character
 #\# #\" #'|#"-reader|)

(defun |#>-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let (chars)
    (do ((curr (read-char stream)
	       (read-char stream)))
	((char= #\newline curr))
      (push curr chars))
    (let* ((pattern (nreverse chars))
	   (pointer pattern)
	   (output))
      (do ((curr (read-char stream)
		 (read-char stream)))
	  ((null pointer))
	(push curr output)
	(setf pointer
	      (if (char= (car pointer) curr)
		  (cdr pointer)
		  pattern))
	(if (null pointer)
	    (return)))
      (coerce
       (nreverse
	(nthcdr (length pattern) output))
       'string))))

(set-dispatch-macro-character
 #\# #\> #'|#>-reader|)

#|
Three substantial difference between CL-PPCRE and Perl.
1. CL-PPCRE is really fast. CL-PPCRE is roughly twice as fast as Perl, often much faster.
performance myth: low level languages result in faster code because you can program closer to the hardware. The more low-level a language is, hte more it prevents you and your compiler from making the effciency optimisations that actually matter.

2. CL-PPCRE  isn't tied to a string-based notation for regular expression.
3. In Perl, regular expression are closely tied into the language. While lisp's syntax is the way it is to accommodate regular expressions and otehr sorts of syntactic shortcuts.

#+ read macro, tests whether it is  CL-PPCRE avaiable before evaluating the following form.
info about reg-expression in Perl.
https://www.tutorialspoint.com/perl/perl_regular_expressions.htm
http://jkorpela.fi/perl/regexp.html
|#

;;CL-PPCRE
(load "C:\\Users\\Spica\\quicklisp\\setup.lisp")
(load "Ch3.lisp")
(ql:quickload :cl-ppcre)

(defun segment-reader (stream ch n)
  (if (> n 0)
      (let ((chars))
	(do ((curr (read-char stream)
		   (read-char stream)))
	    ((char= ch curr))
	  (push curr chars))
	(cons (coerce (nreverse chars) 'string)
	      (segment-reader stream ch (- n 1))))))

#+cl-ppcre
(defmacro! match-mode-ppcre-lambda-form (o!args)
  ``(lambda (,',g!str)
      (cl-ppcre:scan
       ,(car ,g!args)
       ,',g!str)))

#+cl-ppcre
(defmacro! subst-mode-ppcre-lambda-form (o!args)
  ``(lambda (,',g!str)
      (cl-ppcre:regex-replace-all
       ,(car ,g!args)
       ,',g!str
       ,(cadr ,g!args))))

#+cl-ppcre
(defun |#~-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let ((mode-char (read-char stream)))
    (cond
      ((char= mode-char #\m)
       (match-mode-ppcre-lambda-form
	(segment-reader stream
			(read-char stream)
			1)))
      ((char= mode-char #\s)
       (subst-mode-ppcre-lambda-form
	(segment-reader stream
			(read-char stream)
			2)))
      (t (error "Unknown #~~ mode character")))))

#+cl-ppcre
(set-dispatch-macro-character #\# #\~ #'|#~-reader|)

#+cl-ppcre
(defparameter tmp '(defmacro! subst-mode-ppcre-lambda-form (o!args)
  ``(lambda (,',g!str)
      (cl-ppcre:regex-replace-all
       ,(car ,g!args)
       ,',g!str
       ,(cadr ,g!args)))))


#|
Cyclic expression 
Lisp programs are actually not trees but are instead directed acyclic graphs.
#= and ## let you create self-referential S-expression, which allowas you to do things like represent shared branches in directed acyclic graphs.

(defvar shared '(#1=(1) #1#))
*print-circle* should be set to t when serialising cyclic data structures. 
|#

(defun cyclic-p (l)
  (cyclic-p-aux l (make-hash-table)))

(defun cyclic-p-aux (l seen)
  (if (consp l)
      (or (gethash l seen)
	  (progn
	    (setf (gethash l seen) t)
	    (or (cyclic-p-aux (car l) seen)
		(cyclic-p-aux (cdr l) seen))))))

#|
(progn
  (defun ouch ()
    #1=(progn #1#))
    (compile 'ouch))
Lisp stackflow reset......
|#

#|
reader security
Extensibility, the ability to make things happen that weren't originally intended or anticipated. 
However there are times when we would prefer for things to be as inextensible as possible. 
In particular, we don't want outsiders to be as inextensible as possible, which is known as being hacked or rooted.
The largest source of these security problems arise from what programmers joking refer to as impedance mismatch.
Whenever you use something you don't competely understand, there is a possibility you are using it wrong.
Two approaches to combating impedance mismatches: style and understanding
If you just always follow the assumption that lisp does something right, you will hardly ever go wrong. In fact, lisp does this even more right than you might expect.
As in all areas of computer security, you can't consider defence until you have consider defence until you have considered offence. In all other areas of programming, you can arrive at good results constructively, whereas, in security, you must think destructively.
There is no way to attack a program unless you control input to that program in some way.
|#
