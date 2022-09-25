(use util.match)

(define (just x)
  (list x))

(define (none)
  (list))

(define (just? x)
  (and (pair? x)
       (null? (cdr x))))

(define (none? x)
  (null? x))

(define (get-just x)
  (car x))

(define (find-map-just f lst)
  (if (null? lst)
      (none)
      (let ((x (f (car lst))))
	(if (just? x)
	    x
	    (find-map-just f (cdr lst))))))

(define (find-if f list)
  (if (null? list)
      #f
      (let ((v (f (car list))))
	(if v (car list) (find-if f (cdr list))))))

(define (join-string sep strings)
  (define (rec lists)
    (if (null? lists)
	""
	(format #f "~a~a~a" sep (car lists) (rec (cdr lists)))))
  (if (null? strings)
      ""
      (format #f "~a~a" (car strings) (rec (cdr strings)))))

(define (mappend f . args-list)
  (apply append (apply map f args-list)))

(define (remove-last list)
  (reverse (cdr (reverse list))))

(define *scheme-syntax-table* ())
(define *transpiler-temporal-variable* "TRANSPILER_TEMPORAL_VARIABLE")

(define gensym
  (let ((n 0))
    (lambda ()
      (let ((res (string->symbol (format #f "transpiler-gensym-~a" n))))
	(set! n (+ n 1))
	res))))

(define next-temporary-variable
  (let ((num 0))
    (lambda ()
      (let ((res (format #f "~a_~a" *transpiler-temporal-variable* num)))
	(set! num (+ num 1))
	res))))

(define-syntax define-scheme-syntax
  (syntax-rules ()
    ((_ (head . args) then)
     (set! *scheme-syntax-table*
	   (cons
	    (lambda (expr)
	      (match expr
		     (('head . args)
		      (just then))
		     (_ (none))))
	    *scheme-syntax-table*)))))

(define-syntax defmacro
  (syntax-rules ()
    ((_ (head . args) then)
     (set! *scheme-syntax-table*
	   (cons
	    (lambda (expr)
	      (match expr
		     (('head . args)
		      (just (simplify then)))
		     (_ (none))))
	    *scheme-syntax-table*)))))

(define-scheme-syntax (set! var expr)
  `(set! ,(simplify var) ,(simplify expr)))

(defmacro (begin . body)
  `(let () ,@body))

(defmacro (let variables . body)
  `((lambda ,(map car variables) ,@body) ,@(map cadr variables)))

(define-scheme-syntax (lambda args . expr)
  `(lambda ,args ,@(map simplify expr)))

(define-scheme-syntax (define var expr)
  `(define ,var ,(simplify expr)))

(define-scheme-syntax (define var)
  `(define ,var))

(defmacro (define (f . args) . body)
  `(define ,f (lambda ,args ,@body)))

(define-scheme-syntax (aref var . indexes)
  `(aref (simplify ,var) ,@(map simplify indexes)))

(define-scheme-syntax (make-table . binds)
  `(make-table ,@(map (lambda (x) (cons (car x) (simplify (cdr x)))) binds)))

(define-scheme-syntax (if condition then else)
  `(if ,(simplify condition) ,(simplify then) ,(simplify else)))

(defmacro (cond (condition . then) . other)
  `(if ,condition (begin ,@then) (cond ,@other)))

(defmacro (or x . other)
  (let ((tmp (gensym)))
    `(let ((,tmp ,x))
       (if ,tmp ,tmp (or ,@other)))))

(defmacro (or x)
  x)

(defmacro (or)
  #f)

(defmacro (and x . other)
  (let ((tmp (gensym)))
    `(let ((,tmp ,x))
       (if ,tmp (and ,@other) #f))))

(defmacro (and x)
  x)

(defmacro (and)
  #t)

(define-scheme-syntax (cond)
  `(error "lisp cond error: failed all conditions"))

(defmacro (cond ('else . then))
  `(begin ,@then))

(defmacro (not x)
  `(if ,x #f #t))

(define-scheme-syntax (transpiler-eval code)
  code)

(define (simplify-nil expr)
  (if (null? expr)
      (just ())
      (none)))

(define (simplify-boolean expr)
  (if (boolean? expr)
      (just expr)
      (none)))

(define (simplify-symbol expr)
  (if (symbol? expr)
      (just expr)
      (none)))

(define (simplify-number expr)
  (if (number? expr)
      (just expr)
      (none)))

(define (simplify-string expr)
  (if (string? expr)
      (just expr)
      (none)))

(define (simplify-macros expr)
  (find-map-just
   (lambda (f)
     (f expr))
   *scheme-syntax-table*))

(define (simplify-function-call expr)
  (just (map simplify expr)))

(define (simplify expr)
  (get-just (find-map-just
	     (lambda (f)
	       (f expr))
	     (list
	      simplify-nil
	      simplify-boolean
	      simplify-symbol
	      simplify-number
	      simplify-string
	      simplify-macros
	      simplify-function-call))))

(define (read-while-eof)
  (let ((res (read)))
    (if (eof-object? res)
	()
	(cons res (read-while-eof)))))


(map (lambda (line)
       (write line)
       (newline))
     (map simplify (read-while-eof)))
