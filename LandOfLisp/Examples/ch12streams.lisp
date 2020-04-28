#|
Output stream operation:
1. check whether the stream is valid (output-stream-p *standard-output*)
2. push a new item onto the stream (write-char #\x *standard-output*)
|#

#|
Input stream operation:
1. Check whether the stream is valid (input-stream-p *standard-input*)
2. Pop an item off of the stream (read-char *standard-input*)
|#

#|
Working with files
(with-open-file (my-stream "data.txt" :direction :output)
  (print "my data" my-stream))
:if-exists :error :supersede(overwritten)


(with-open0file (my-stream "data.txt" :direction :input)
  (read my-stream))
|#

#|
Working with string streams
(defparameter foo (make-string-output-stream))
(princ "asd" foo)
(princ "qweqweqw" foo)
(get-output-stream-string foo)
|#

#|
with-output-to-string
intercept any text that would otherwise be output to the console.
(with-output-to-string (*standard-output*)
  (princ "the sum of ")
  (princ 5)
  (princ " and ")
  (princ 2)
  (princ " is ")
  (princ (+ 2 5)))
> "the sum of 5 and 2 is 7"
|#
