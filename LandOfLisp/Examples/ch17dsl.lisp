(load "ch16macro.lisp")

(defun print-tag (name alst closingp)
  (princ #\<)
  (when closingp
    (princ #\/))
  (princ (string-downcase name))
  (mapc (lambda (att)
          (format t " ~a=\"~a\"" (string-downcase (car att)) (cdr att)))
        alst)
  (princ #\>))

(defmacro tag (name atts &body body)
  `(progn (print-tag ',name
                     (list ,@(mapcar (lambda (x) `(cons ',(car x) ,(cdr x)))
                                     (pairs atts)))
                     nil)
          ,@body
          (print-tag ',name nil t)))

(defmacro html (&body body)
  `(tag ()
       ,@body))

(defmacro body (&body body)
  `(tag ()
       ,@body))

(defmacro svg (&body body)
  `(tag svg  (xmlns "http://www.w3.org/2000/svg"
                    "xmlns:xlink" "http://www.w3.org/1999/xlink")
     ,@body))


(defun brightness (col amt)
  (mapcar (lambda (x) (min 255 (max 0 (+ x amt)))) col))

(defun svg-style (color)
  (format nil
          "~{fill:rgb(~a,~a,~a);stroke:rgb(~a,~a,~a)~}"
          (append color (brightness color -100))))

(defun circle (center radius color)
  (tag circle (cx (car center)
                  cy (cdr center)
                  r radius
                  style (svg-style color))))

(defun polygon (points color)
  (tag polygon (points (format nil "~{~a,~a ~}"
                               (mapcan (lambda (tp)
                                         (list (car tp) (cdr tp)))
                                       points))
                       style (svg-style color))))

(defun random-walk (value length)
  (labels ((f (value length acc)
             (if (zerop length)
                 (reverse acc)
                 (f (if (zerop (random 2))
                        (1- value)
                        (1+ value))
                    (1- length)
                    (cons value acc)))))
    (f value length nil)))

(defun random-svg ()
  (with-open-file (*standard-output* "random_walk.svg"
                                     :direction :output
                                     :if-exists :supersede)
    (svg (loop repeat 10
               do (polygon (append '((0 . 200))
                                   (loop for x from 1 to 400
                                         for y in (random-walk 100 400)
                                         collect (cons x y))
                                   '((400 . 200)))
                           (loop repeat 3 collect
                                 (random 256)))))))
