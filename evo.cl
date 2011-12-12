(LOAD "utils.cl")
(load "tfbs.cl")

(defparameter fitness-penalty 1000000)
(defparameter crossover-prob .9)
(defparameter replicate-prob .09)
(defparameter mutation-prob .01)
(defparameter elitism t)

(defun make-val ()
  (make-exp-of-type answer-type))

(defun make-exp-of-type (type)
  (let* ((f (get-func-with-return-type type))
	 (args (mapcar #'make-exp-of-type (arguments-of f)))
	 (expr (append (list f) args)))
    (if (member f (append constants variables))
	(first expr)
	expr)))

(defun my-type-of (f)
  (lookup f types))

(defun return-type-of (f)
  (first (last (lookup f types))))

(defun arguments-of (f)
  (reverse (rest (reverse (my-type-of f)))))

(defun get-func-with-return-type (type)
  (let ((candidates (remove-if-not 
		     (lambda (pair) 
		       (equal (first (last (second pair))) type)) types)))
    (choose-randomly (mapcar #'first candidates))))

(defun make-population (n)
  (loop for x from 1 to n
     collect (make-val)))

(defun bind-var (p sym val)
  (if (listp val)
      `(let ((,sym (list ,@val)))
	 ,p)
      `(let ((,sym ,val))
	 ,p)))

(defun bind-vars (p sym val &rest args)
  (if (> (length args) 1)
      (let ((p-prime `(let ((,sym ,val)) ,p))
	    (sym-prime (nth 0 args))
	    (val-prime (nth 1 args))
	    (args-prime (subseq args 2)))
	(bind-vars p-prime sym-prime val-prime args-prime))
      (bind-var p sym val)))
  
(defun evaluate (p vars)
  (handler-case
      (eval (bind-var p 'input vars))
  (error (e) fitness-penalty)))

(defun evaluate-problems (p)
  (mapcar (lambda (x) (evaluate p x))  problems))

(defun descend (tree &optional (path nil))
  (if (atom tree) 
      (list tree path)
	(let* ((n (random (length tree))))
	  (if (= n 0)
	      (list tree path)
	      (descend (nth n tree) (append path (list n)))))))

(defun stitch (tree leaf path)
  (if path
      (let* ((i (car path))
	     (new-path (cdr path))
	     (pre (subseq tree 0 i))
	     (new-tree (nth i tree))
	     (post (subseq tree (+ i 1) (length tree))))
	`(,@pre ,(stitch new-tree leaf new-path) ,@post))
      leaf))

(defun crossover (population)
  "accepts a population and returns crossed-over mutants"
  (let* ((p (tournament-select population))
	 (q (tournament-select population))
	 (p-leaf-path (descend p))
	 (q-leaf-path (descend q))
	 (p-leaf (first p-leaf-path))
	 (q-leaf (first q-leaf-path))
	 (p-type (return-type-of (head p-leaf)))
	 (q-type (return-type-of (head q-leaf))))
    (if (equal p-type q-type)
	(let ((p-path (second p-leaf-path))
	      (q-path (second q-leaf-path)))
	  (print p-type)
	  (print q-type)
	  (list (stitch p q-leaf p-path)
		(stitch q p-leaf q-path)))
	(crossover population))))

(defun mutate (p)
  (let ((path (second (descend p))))
    (stitch p (make-val) path)))

(defun pick-winners (population)
  (let ((sorted-pop (sort population #'< :key #'fitness)))
    (subseq sorted-pop 0 (/ (length sorted-pop) 2))))

(defun select-by-fitness (population fits);pass fitnesses in as a parameter to avoid needless recomputation
  (let* ((cumfits (discrete-cmf (normalize (mapcar (lambda (x) (/ 1 x)) fits))))
	 (x (random 1.0)))
    (caar (remove-if-not (lambda (pair) (>= (second pair) x)) 
			(mapcar #'list population cumfits)))))

(defun tournament-select (population)
  (let ((p (choose-randomly population))
	(q (choose-randomly population)))
    (first (sort (list p q) #'< :key #'fitness))))

(defun make-child (population)
  (let ((selector (random 1.0)))
	(cond ((< selector crossover-prob)
	       (car (crossover population)))
	      ((< selector (+ crossover-prob replicate-prob))
	       (tournament-select population))
	      (t (mutate (tournament-select population))))))

(defun update-pop (population) 
  (if elitism
      (cons (best population) (loop for i from 2 to (length population)
				 collect (make-child population)))
      (loop for i from 1 to (length population)
	 collect (make-child population))))

(defun avg-fitness (population)
  (mean (mapcar #'floor (fitnesses population)))); take floor

(defun fit-summary (population)
  (let ((fits (fitnesses population)))
    (mapcar #'float 
	    (list (reduce #'min fits) (mean fits)))))

(defun fitnesses (population)
  (mapcar #'fitness population))

(defun best-fitness (population)
  (fitness (best population)))

(defun iterate (population)
  (let ((round-winner (best population)))
    (if (= 0 (fitness round-winner))
      round-winner
      (progn
	(print (avg-fitness population))
	(iterate (update-pop population))))))

(defun best (population)
  (car (sort population #'< :key #'fitness)))


(defun complexity-stats (pop)
  (let ((fits (fitnesses pop))
	(depths (mapcar #'depth pop))
	(sizes (mapcar #'size pop)))
    (mapcar #'float (list (apply #'min fits) 
			  (mean fits)
			  (variance fits)
			  (mean depths) 
			  (variance depths)
			  (mean sizes)
			  (variance sizes)))))

(defun display-pop-fits (pop)
  (sort (mapcar #'float (fitnesses pop)) #'<))