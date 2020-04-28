#|
(format "the destination parameter" "the control string" value parameter)
control sequence
1. ~$ monetary floating-point "~$" 1.5 -> 1.50
2. ~s behave like prin1 "~s" "foo" -> "foo"
3. ~a like princ "~a" "foo" -> foo
4. padded with spaces on the right, ~na. ~10a foo -> foo        (10 chars in total)
5. padded with spaces on the left, ~n@a. ~10@a foo ->        foo (10 chars in total)
6. add more parameters, ~10,3a , 10 chars on right, 3 spaces added each time. ~10,3a foo -> foo          (12 chars in total)
7. set exact numbe of padding ~,,4a (4 sapces on right)
8. add specified char rather than space ~,,3,'!a foo -> foo!!! ;  ~,,4,'#@a foo -> ####foo
|#

#|
for integers
1. ~x hexadecimal
2. ~b binary
3. ~d decimal
4. ~:d 1000000 -> 1,000,000
5. also can be applyed with padding
|#

#|
for floating-point numbers
1. ~f
2. ~nf automatically round the value with n length (including the dot)
3. ~,4f change the number of displayed after decimal point
4. ~,,2f scale the result. ~,,2f 0.77 -> 77.0
|#

#|
print new line
(terpri) start a new line
(fresh-line) start a new line if needed. (doing nothing if there is in a new line)
~% -> terpri, ~5% 5 new lines
~& -> fresh-line
|#

#|
(loop repeat 10 do (format t "~30:@<~a~;~a~;~a~>~%") (random-str) (random-str) (random-str))
(loop repeat 10 do (format t "~10:@<~a~>~10:@<~a~>~10:@<~a~>~%") (random-str) (random-str) (random-str))
|#

#|
iterting through lists
~{~}
(format t "~{I have ~a! ~}" (loop repeat 10 collect (random-str)))
|#

#|
(format t "|~{~<|~%|~,33:;~2d ~>~}|" (loop for x below 100 collect x))
|#

(defun robots ()
  (loop named main
        with directions ='((q . -65) (w . -64) (e . -63) (a . -1)
                           (d . 1) (z . 63) (x . 64) (c . 65))
        for pos = 544
          then (progn (format t "~%qwe/asd/zxc to move, (t)eleport, (l)eave:")
                      (force-output)
                      (let* ((c (read))
                             (d (assoc c directions)))
                        (cond (d (+ pos (cdr d)))
                              ((eq 't c) (random 1024))
                              ((eq 'l c) (return-from main 'bye))
                              (t pos))))
        for monsters = (loop repeat 10 collect (random 1024))
          then (loop for mpos in monsters
                     collect (if (> (count mpos monsters) 1)
                                 mpos
                                 (cdar (sort (loop for (k . d) in directions
                                                   for new-mpos = (+ mpos d)
                                                   collect (cons (+ (abs (- (mod new-mpos 64)
                                                                            (mod pos 64)))
                                                                    (abs (- (ash new-mpos -6)
                                                                            (ash pos -6))))
                                                                 new-mpos))
                                             '<
                                             :key #'car))))
        when (loop for mpos in monsters always (> (count mpos monsters) 1))
          return 'player-wins
        do (format t
                   "~%|~{~<|~%|~,65:;~A~>~}|"
                   (loop for p below 1024
                         collect (cond ((member p monsters)
                                        (cond ((= p pos) (return-from main 'player-loses))
                                              ((> (count p monsters) 1) #\#)
                                              (t #\A)))
                                       ((= p pos) #\@)
                                       (t #\ ))))))
