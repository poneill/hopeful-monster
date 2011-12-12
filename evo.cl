(load "utils.cl")
(defvar unary-funcs '(sqrt floor ceiling))
(defvar list-constants '(t ()))
(defvar unary-list-funcs '(car cdr))
(defvar binary-list-funcs '(=))
(defvar ternary-list-funcs '(if))
(defvar list-variables '(xs))
(defvar binary-funcs '(+ - * / expt))
(defvar constants '(pi e 1 2 3 4 5))
(defun f (x)
  (+ (expt x 4) (expt x 3) (expt x 2) (expt x 1) 1))
;;(defvar problems '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
;;(defvar answers (mapcar #'f problems))
;;(defvar list-answers (mapcar #'reverse list-problems))
(defvar fitness-penalty 1000000)



(defun sum (xs)
  (apply #'+ xs))
(defun fib (n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(defun square (x)
  (* x x))

(defun clear-zeros (xs)
  (mapcar (lambda (x) (if (= x 0) 1 x)) xs))

(defun list-fitness (p)
  (let ((responses (mapcar (lambda (x) (list-evaluate p x)) problems)))
    (sum (mapcar (lambda (xy) (apply #'compare-lists xy)) (zip responses list-answers)))))
  
(defun compare-lists (xs ys);lower scores are better
  (max (sum (mapcar (lambda (x y) 
		      (if (= x y) 0 1)) 
		    (zip xs ys)))
       (length ys)))

(defun fitness (p)
  (handler-case 
      (let* ((responses (mapcar (lambda (x) (evaluate p x)) problems))
	     (fit (sum (zipwith #'/ (mapcar #'square
					    (zipwith #'- responses answers)) 
				(clear-zeros answers)))))
	(if (and (realp fit) (< fit fitness-penalty))
	    fit
	    fitness-penalty))
    (error (e) fitness-penalty)))

(defun choose-randomly (lst)
  (nth (random (length lst)) lst))

(defun make-val ()
  (eval (list (choose-randomly '(make-unary-func
				 make-binary-func
				 make-constant
				 make-variable)))))

(defun make-list-val ()
    (eval (list (choose-randomly '(make-unary-list-func
				   make-binary-list-func
				   make-ternary-list-func
				   make-list-constant
				   make-list-constant
				   make-list-constant
				   make-list-variable
				   call-program)))))
(defun make-constant ()
  (choose-randomly constants))

(defun make-list-constant ()
  (choose-randomly list-constants))

(defun make-list-variable ()
  (choose-randomly list-variables))

(defun make-variable ()
  (choose-randomly list-variables))

(defun make-unary-func ()
  (let ((f (choose-randomly unary-funcs)))
    (list f (make-val))))

(defun make-unary-list-func ()
  (let ((f (choose-randomly unary-list-funcs)))
    (list f (make-list-val))))

(defun make-binary-func ()
  (let ((f (choose-randomly binary-funcs)))
    (list f (make-val) (make-val))))

(defun make-binary-list-func ()
  (let ((f (choose-randomly binary-list-funcs)))
    (list f (make-list-val) (make-list-val))))

(defun make-ternary-list-func ()
  (let ((f (choose-randomly ternary-list-funcs)))
    (list f (make-list-val) (make-list-val) (make-list-val))))

(defun call-program ()
  (list 'p (make-list-val)))

(defun make-list-population (n)
  (loop for x from 1 to n
     collect (make-list-val)))

(defun bind-var (p sym val)
  `(let ((,sym ,val))
     ,p))

(defun list-bind-var (p sym val)
  `(let ((,sym ',val))
     ,p))

(defun bind-vars (p sym val &rest args)
  (if (> (length args) 1)
      (let ((p-prime `(let ((,sym ,val)) ,p))
	    (sym-prime (nth 0 args))
	    (val-prime (nth 1 args))
	    (args-prime (subseq args 2)))
	(bind-vars p-prime sym-prime val-prime args-prime))
      (bind-var p sym val)))
  
(defun evaluate (p r)
  (handler-case
      (eval (bind-var p 'r r))
  (error (e) fitness-penalty)))

(defun list-evaluate (p xs)
  (handler-case
      (eval (attach-recursive-definition
	     p
	     (list-bind-var p 'xs xs)))
  (error (e) '())))

(defun attach-recursive-definition (p bound-program)
  `(progn
     (defun p (xs)
       (if (listp xs)
       ,p
       nil))
     ,bound-program))

(defun evaluate-problems (p)
  (mapcar (lambda (x) (evaluate p x))  problems))

(defun zip (xs ys)
  (if (or (null xs) (null ys))
      nil
      (cons (list (car xs) (car ys)) (zip (cdr xs) (cdr ys)))))

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

(defun crossover (p q)
  "accept two programs, p and q, and return crossed-over mutants"
  (let* ((p-leaf-path (descend p))
	 (q-leaf-path (descend q))
	 (p-leaf (first p-leaf-path))
	 (p-path (second p-leaf-path))
	 (q-leaf (first q-leaf-path))
	 (q-path (second q-leaf-path)))
    (list (stitch p q-leaf p-path)
	  (stitch q p-leaf q-path))))

(defun mutate (p)
  (let ((path (second (descend p))))
    (stitch p (make-val) path)))

(defun pick-winners (population)
  (let ((sorted-pop (sort population #'< :key #'fitness)))
    (subseq sorted-pop 0 (/ (length sorted-pop) 2))))

(defun update-pop (population)
  (let* ((winners (pick-winners population))
	 (num-children (- (length population) (length winners)))
	 (children (mapcar (lambda (x) (let* ((p (choose-randomly winners))
					      (q (choose-randomly winners)))
					 (mutate (car (crossover p q)))))
			   (loop for i from 1 to num-children collect i))))
    (append winners children)))

(defvar crossover-prob .9)
(defvar replicate-prob .09)
(defvar mutation-prob .01)
(defvar elitism t)

(defun discrete-cmf (pmf &optional (acc 0))
  "Return the cdf of a pmf"
  (if pmf
      (let ((acc-prime (+ (car pmf) acc)))
	(cons acc-prime (discrete-cmf (cdr pmf) acc-prime)))))

(defun select-by-fitness (population fits);pass fitnesses in as a parameter to avoid needless recomputation
  (let* ((cumfits (discrete-cmf (normalize (mapcar (lambda (x) (/ 1 x)) fits))))
	 (x (random 1.0)))
    (caar (remove-if-not (lambda (pair) (>= (second pair) x)) 
			(mapcar #'list population cumfits)))))

(defun tournament-select (population)
  (let ((p (choose-randomly population))
	(q (choose-randomly population)))
    (first (sort (list p q) #'< :key #'fitness))))

(defun list-tournament-select (population)
  (let ((p (choose-randomly population))
	(q (choose-randomly population)))
    (first (sort (list p q) #'< :key #'list-fitness))))

(defun normalize (xs)
  (mapcar (lambda (x)(/ x (sum xs))) xs))

(defun make-child (population)
  (let ((selector (random 1.0)))
	(cond ((< selector crossover-prob)
	       (let ((program1 (tournament-select population))
		     (program2 (tournament-select population)))
		 (car (crossover program1 program2))))
	      ((< selector (+ crossover-prob replicate-prob))
	       (tournament-select population))
	      (t (mutate (tournament-select population))))))

(defun update-pop2 (population) 
  (if elitism
      (cons (best population) (loop for i from 2 to (length population)
				 collect (make-child population)))
      (loop for i from 1 to (length population)
	 collect (make-child population))))

(defun list-update-pop (population) 
  (append (loop for i from (if elitism 1 2) to (length population)
	     collect (make-child population))
	  (if elitism (best population) nil)))


(defun mean (xs)
  (/ (sum xs) (length xs)))

(defun avg-fitness (population)
  (mean (mapcar #'floor (fitnesses population)))); take floor

(defun fit-summary (population)
  (let ((fits (fitnesses population)))
    (mapcar #'float 
	    (list (reduce #'min fits) (mean fits)))))

(defun fitnesses (population)
  (mapcar #'fitness population))

(defun list-fitnesses (population)
  (mapcar #'list-fitness population))

(defun best-fitness (population)
  (fitness (best population)))

(defun safe-mean (xs &optional (acc 0) (n 0)); sometimes we need to compute very large means
  (if xs
      (let* ((x (car xs))
	     (rest (cdr xs))
	     (acc-prime (/ (+ (* acc n) x) (+ n 1))))
	(print n)
	(safe-mean rest acc-prime (+ n 1)))
      acc))

(defun iterate (population)
  (let ((round-winner (best population)))
    (if (= 0 (fitness round-winner))
      round-winner
      (progn
	(print (avg-fitness population))
	(iterate (update-pop population))))))

(defun best (population)
  (car (sort population #'< :key #'fitness)))

(defun iterate-function (f n args)
  (if (= n 1)
      (funcall f args)
      (iterate-function f (- n 1) (funcall f args))))
				  
(defun zipwith (f xs ys)
  (if (or (null xs) (null ys))
      ()
      (cons (funcall f (car xs) (car ys))
	    (zipwith f (cdr xs) (cdr ys)))))

(defun depth (tree)
  (if (atom tree)
      0
      (+ 1 (apply #'max (mapcar #'depth tree)))))

(defun size (tree)
  (if (atom tree)
      1
      (+ (length tree) (sum (mapcar #'depth tree)))))

(defun variance (xs)
  (- (mean (mapcar #'square xs)) (square (mean xs))))

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