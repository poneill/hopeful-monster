(load "utils.lisp")
(load "tfbs.lisp")

(defparameter fitness-penalty 1000000)
(defparameter crossover-prob .9)
(defparameter replicate-prob .09)
(defparameter mutation-prob .01)
(defparameter elitism t)
(defparameter tournament-selector 4)

(defun get-fitness (p)
  "select fitness from p"
  (if (= (length p) 2)
      (second p)
      (error "called get-fitness with wrong args")))

(defun get-program (p)
  "select program from p"
  (if (= (length p) 2)
      (first p)
      (error "called get-program with wrong args")))

(defun attach-fitness (p)
  (list p (fitness p)))

(defun make-program ()
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
     collect (attach-fitness (make-program))))

(defun bind-var (p sym val)
  (if (listp val)
      `(let ((,sym (list ,@val)))
	 ,sym
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
      (eval 
       (bind-var p 'input vars))
  (error () fitness-penalty)))

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
	(let* ((p-path (second p-leaf-path))
	      (q-path (second q-leaf-path))
	      (p-prime (stitch p q-leaf p-path)))
	  (list p-prime
		(stitch q p-leaf q-path))) ;currently we throw this
					   ;value away, so why bother
					   ;evaluating fitness?
					   ;Ultimately we need to make
					   ;a decision about this: use
					   ;the second crossover
					   ;offspring, or stop
					   ;computing them
	(crossover population))))

(defun mutate (p)
  (let* ((descent (descend p))
	 (leaf (second descent))
	 (path (second descent))
	 (mutant (make-program)))
    (if (equal (return-type-of (head leaf))
	       (return-type-of (head mutant)))
	(stitch p mutant path)
	(mutate p))))

(defun tournament-select (population)
  (let ((candidates (loop for i from 1 to tournament-selector 
		       collect (choose-randomly population))))
    (get-program (first (sort candidates #'< :key #'get-fitness)))))

(defun make-child (population)
  (let* ((selector (random 1.0))
	 (p (cond ((< selector crossover-prob)
		   (car (crossover population)))
		  ((< selector (+ crossover-prob replicate-prob))
		   (tournament-select population))
		  (t (mutate (tournament-select population))))))
    (attach-fitness p)))

(defun update-pop (population) 
  (if elitism
	(cons (best population) (loop for i from 2 to (length population)
				   collect (make-child population)))
      (complexity-stats (loop for i from 1 to (length population)
	 collect (make-child population)))))

(defun avg-fitness (population)
  (mean (mapcar #'floor (fitnesses population)))); take floor

(defun fit-summary (population)
  (let ((fits (fitnesses population)))
    (mapcar #'float 
	    (list (reduce #'min fits) (mean fits)))))

(defun fitnesses (population)
  (mapcar #'get-fitness population))

(defun best-fitness (population)
  (get-fitness (best population)))

(defun iterate (population)
  (let ((round-winner (best population)))
    (if (= 0 (fitness round-winner))
      round-winner
      (progn
	(print (complexity-stats population))
	(iterate (update-pop population))))))

(defun best (population)
  (let ((winner (argmin population #'get-fitness)))
    (print (get-fitness winner))
    winner))

(defun complexity-stats (pop)
  (let* ((fits (fitnesses pop))
	(depths (mapcar #'depth pop))
	(sizes (mapcar #'size pop))
	(stats (mapcar #'float (list (length fits)
				     (apply #'min fits) 
				     (mean fits)
				     (variance fits)
				     (mean depths) 
				     (variance depths)
				     (mean sizes)
				     (variance sizes)))))
    (print stats)
    pop))

(defun display-pop-fits (pop)
  (sort (mapcar #'float (fitnesses pop)) #'<))