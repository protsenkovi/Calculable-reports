(defun euler(f a b n y0 log-p)
  (let* ((result (list 
				   (list "$i$")
				   (list "$x_n$")
				   (list "$y_n$")
				   (list "$f(x,y)$")
				   (list "$y_{n + 1}$")))
		 (deltaX (* (- b a) (/ 1.0 n))))
	 (loop for i from 1 upto (1+ n)
		   for x from a by deltaX
		   with y = y0
		   with nexty
		   do
		   (setf nexty (+ y (* deltaX (funcall f x y))))
		   (nconc (elt result 0) (list i))
		   (nconc (elt result 1) (list x))
		   (nconc (elt result 2) (list y))
		   (nconc (elt result 3) (list (funcall f x y)))
		   (nconc (elt result 4) (list nexty))
		   (setf y nexty))
	 (if log-p result (subseq (elt result 2) 1))))

(defun runge-kutta (f a b n y0 log-p)
  (let* ((result (list 
				   (list "$i$")
				   (list "$x_n$")
				   (list "$y_n$")
				   (list "$k_1$")
				   (list "$k_2$")
				   (list "$k_3$")
				   (list "$k_4$")
				   (list "$y_{n + 1}$")))
		 (deltaX (* (- b a) (/ 1.0 n))))
	 (loop for i from 1 upto (1+ n)
		   for x from a by deltaX
		   with y = y0
		   with nexty
		   with k1
		   with k2
		   with k3
		   with k4
		   do
		   (setf k1 (* deltaX (funcall f x y)))
		   (setf k2 (* deltaX (funcall f (+ x (/ deltaX 2)) (+ y (* (/ k1 2) )))))
		   (setf k3 (* deltaX (funcall f (+ x (/ deltaX 2)) (+ y (* (/ k2 2) )))))
		   (setf k4 (* deltaX(funcall f (+ x deltaX) (+ y k3))))
		   (setf nexty (+ y (* 1/6 (+ k1 (* 2 k2) (* 2 k3) k4))))
		   (nconc (elt result 0) (list i))
		   (nconc (elt result 1) (list x))
		   (nconc (elt result 2) (list y))
		   (nconc (elt result 3) (list k1))
		   (nconc (elt result 4) (list k2))
		   (nconc (elt result 5) (list k3))
		   (nconc (elt result 6) (list k4))
		   (nconc (elt result 7) (list nexty))
		   (setf y nexty))
	 (if log-p result (subseq (elt result 2) 1))))

(defun source-fun (f a b n y0 log-p)
  (let* ((result (list 
				   (list "$i$")
				   (list "$x_n$")
				   (list "$y_n$")))
		 (deltaX (* (- b a) (/ 1.0 n))))
	 (loop for i from 1 upto (1+ n)
		   for x from a by deltaX
		   with y = y0
		   do
		   (setf y (funcall f x))
		   (nconc (elt result 0) (list i))
		   (nconc (elt result 1) (list x))
		   (nconc (elt result 2) (list y)))
	 (if log-p result (subseq (elt result 2) 1))))

(defun method-error(x1-li x2-li)
  (loop for x1 in x1-li
		for x2 in x2-li
		for i from 1
		summing (expt (- x1 x2) 2) into s 
		finally (return (sqrt (/ s i)))))
