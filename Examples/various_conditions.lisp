(defun tifd (para)
  (if para (princ "t") (princ "f")))

(defun tifs (para)
  (if para (princ "t")))

(defun twhen (para)
  (when para (princ "t\n") (princ "t\n")))

(defun tunl (para)
  (unless para (princ "f\n") (princ "f\n")))

(defun tcond (para)
  (cond ((eq para 'a) (princ "conda\n"))
	((eq para 'b) (princ "condb\n"))
	(t (princ "condt\n"))))

(defun tcase (para)
  (case para
    ((a) (princ "casea\n"))
    ((b) (princ "caseb\n"))
    (otherwise (princ "caset\n"))))
