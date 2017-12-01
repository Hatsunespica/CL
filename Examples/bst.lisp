(defstruct (node (:print-function
		  (lambda (n s d)
		    (format s "#<~A>" (node-elt n)))))
  elt (l nil) (r nil))

(defun bst-insert (obj bst <)
  (if (null bst)
      (make-node :elt obj)
      (let ((elt (node-elt bst)))
	(if (eql  obj elt)
	    bst
	    (if (funcall < obj elt)
		(make-node
		 :elt elt
		 :l (bst-insert obj (node-l bst) <)
		 :r (node-r bst))
		(make-node
		 :elt elt
		 :r (bst-insert obj (node-r bst) <)
		 :l (node-l bst)))))))

(defun bst-find (obj bst <)
  (if (null bst)
      nil
      (let ((elt (node-elt bst)))
	(if (eql obj elt)
	    bst
	    (if (funcall < obj elt)
		(bst-find obj (node-l bst) <)
		(bst-find obj (node-r bst) <))))))

(defun bst-min (bst)
  (and bst
       (or (bst-min (node-l bst)) bst)))

(defun bst-max (bst)
  (and bst
       (or (bst-max (node-r bst)) bst)))
	    
(defun bst-remove (obj bst <)
  (if (null bst)
      nil
      (let ((elt (node-elt bst)))
	(if (eql obj elt)
	    (percolate bst)
	    (if (funcall < obj elt)
		(make-node
		 :elt elt
		 :l (bst-remove obj (node-l bst) <)
		 :r (node-r bst))
		(make-node
		 :elt elt
		 :r (bst-remove obj (node-r bst) <)
		 :l (node-l bst)))))))

(defun percolate (bst)
  (cond ((null (node-l bst))
	 (if (null (node-r bst))
	     nil
	     (rperc bst)))
	((null (node-r bst)) (lperc bst))
	(t (if (zerop (random 2))
	       (lperc bst)
	       (rperc bst)))))

(defun rperc (bst)
  (make-node :elt (node-elt (node-r bst))
	     :l (node-l bst)
	     :r (percolate (node-r bst))))

(defun lperc (bst)
  (make-node :elt (node-elt (node-l bst))
	     :l (percolate (node-l bst))
	     :r (node-r bst)))

(defun bst-traverse (fn bst)
  (when bst
    (bst-traverse fn (node-l bst))
    (funcall fn (node-elt bst))
    (bst-traverse fn (node-r bst))) 
