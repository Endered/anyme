(define (myprint x)
  (cps-call console.log x))

(define (print-list x)
  (cond ((list? x)
	 (myprint "(")
	 (map print-list x)
	 (myprint ")"))
	(else
	 (myprint x))))

(defmacro (define-struct name . elements)
  (let ((struct-name (format #f "lisp-struct-~a" name))
	(name/ (string->symbol (format #f "~a-" name))))
    `(begin
       (define (,name ,@elements)
	 (list ,struct-name ,@elements))
       (define (,name/ x)
	 (if (= (car x) ,struct-name)
	     (cdr x)
	     #f)))))

(defmacro (match1 x symbol . then)
  `(let ((,symbol ,x))
     (cons (begin ,@then) ())))

(defmacro (match1 x () . then)
  (let ((evaled (gensym)))
    `(let ((,evaled ,x))
       (if (null? ,evaled)
	   (cons (begin ,@then) ())
	   #f))))

(defmacro (match1 x (pattern1 . pattern2) . then)
  (let ((evaled (gensym))
	(result1 (gensym))
	(result2 (gensym)))
    `(let ((,evaled ,x))
       (if (pair? ,evaled)
	   (let ((,result2  (match1 (car ,evaled) ,pattern1
				    (let ((,result1 (match1 (cdr ,evaled) ,pattern2
							    ,@then)))
				      (if (and ,result1 (car ,result1)) (car ,result1) #f)))))
	     (if (and ,result2 (car ,result2)) ,result2 #f))
	   #f))))

(defmacro (match x . patterns)
  (let ((evaled (gensym)))
    `(let ((,evaled ,x))
       (car (or ,@(map (lambda (pattern) `(match1 ,evaled ,@pattern)) patterns))))))


(myprint (match (list 1 2 (list 3 4))
		((x y z a) (* x y z a))
		((x y (z a)) (+ x y z a))))





(defmacro (pattern-match1 x pattern . then)
  `(let ((,pattern ,x))
     (cons (begin ,@then) ())))

(defmacro (pattern-match1 x (struct-name . elements) . then)
  (let ((binds (map (lambda (x) (if (symbol? x) x (gensym))) elements))
	(struct-name/ (string->symbol (format #f "~a-" struct-name)))
	(result (gensym)))
    (define (rec binds elements)
      (cond ((null? elements)
	     `(begin ,@then))
	    ((symbol? (car elements))
	     (rec (cdr binds) (cdr elements)))
	    (else
	     (let ((result (gensym)))
	       `(let ((,result (pattern-match1 ,(car binds) ,(car elements)
					       ,(rec (cdr binds) (cdr elements)))))
		  (if (and ,result (car ,result)) (car ,result) #f))))))
    `(let ((,result (match1 (,struct-name/ ,x) ,binds
			    ,(rec binds elements))))
       (if (and ,result (car ,result)) ,result #f))))

(defmacro (pattern-match x . patterns)
  (let ((evaled (gensym)))
    `(let ((,evaled ,x))
       (car (or ,@(map (lambda (pattern) `(pattern-match1 ,evaled ,@pattern)) patterns))))))



(define-struct Cons x y)
(define-struct Nil)

(myprint
 (pattern-match (Cons 1 (Cons 2 (Nil)))
		((Cons x (Cons y (Cons z (Nil)))) (+ x y z))
		((Cons x (Cons y (Nil))) (+ x y))
		((Cons x (Nil)) x)
		((Nil) 0)))
