#|
Signaling a condition
(error "foo")
> *** - foo
The following restarts are available:
ABORT          :R1      Abort main loop
|#

#|
Creating custom conditions
(define-condition foo () ()
  (:report (lambda (condition stream)
    (princ "Stop FOOing around, numbskull!" stream))))

> *** - Stop FOOing around, numbskull!
The following restarts are available:
ABORT          :R1      Abort main loop
|#

#|
Intercepting conditions
(defun bad-function ()
  (error 'foo))

(handler-case (bad-function)
  (foo () "somebody signaled foo!")
  (bar () "somebody signaled bar!"))
|#

#|
Protecting resources against unexpected conditions

(unwind-protect (/ 1 0)
  (princ "I need to say 'flubyduby' matter what"))

> *** - /: division by zero
The following restarts are available:
ABORT          :R1      Abort main loop
> :r1
I need to say 'flubyduby' matter what

-----------------------
the program still prints its crucial message even after aborting the program.
"with-" macros would unwind-protect under the hood so you don't need to write unwind-protect manually.
|#

(defun http-char (c1 c2 &optional (default #\Space))
  (let ((code (parse-integer
               (coerce (list c1 c2) 'string)
               :radix 16
               :junk-allowed t)))
    (if code
        (code-char code)
        default)))

(defun decode-param (s)
  (labels ((f (lst)
             (when lst
               (case (car lst)
                 (#\% (cons (http-char (cadr lst) (caddr lst))
                            (cdddr lst)))
                 (#\+ (cons #\space (f (cdr lst))))
                 (otherwise (cons (car lst) (f (cdr lst))))))))
    (coerce (f (coerce s 'list)) 'string)))

(defun parse-params (s)
  (let* ((i1 (position #\= s))
         (i2 (position #\& s)))
    (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
                          (decode-param (subseq s (1+ i1) i2)))
                    (and i2 (parse-params (subseq s (1+ i2))))))
          ((equal s "") nil)
          (t s))))

(defun parse-url (s)
  (let* ((url (subseq s
                      (+ 2 (position #\space s))
                      (position #\space s :from-end t)))
         (x (position #\? url)))
    (if x
        (cons (subseq s 0 x) (parse-params (subseq url (1+ x))))
        (cons url nil))))

(defun get-header (stream)
  (let* ((s (read-line stream))
         (h (let ((i (position #\: s)))
              (when i
                (cons (intern (string-upcase (subseq s 0 i)))
                      (subseq s (+ i 2)))))))
    (when h
      (cons h (get-header stream)))))

(defun get-contect-params (stream header)
  (let ((length (cdr (assoc 'content-length header))))
    (when length
      (let ((content (make-string (parse-integer length))))
        (read-sequence content stream)
        (parse-params content)))))

(defun serve (request-handler)
  (let ((socket (socket-server 8080)))
    (unwind-protect
         (loop (with-open-stream (stream (socket-accept socket))
                 (let* ((url (parse-url (read-line stream)))
                        (path (car url))
                        (header (get-header stream))
                        (params (append (cdr url)
                                        (get-contect-params stream header)))
                        (*standard-output* stream))
                   (funcall request-handler path header params))))
      (socket-server-close socket))))

(defun hello-request-handler (path header params)
  (if (equal path "greeting")
      (let ((name (assoc 'name params)))
        (if (not name)
            (princ "<html><form>What is your name?<input name='name' /></form></html>")
            (format t "<html>Nice to meet you, ~a!</html>" (cdr name))))
      (princ "Sorry... I don't know that page.")))


