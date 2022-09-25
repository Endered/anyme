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

(define *transpiler-macros* ())

(define-syntax define-transpiler-syntax
  (syntax-rules ()
    ((_ (head . args) then)
     (set! *transpiler-macros*
	   (cons
	    (lambda (expr)
	      (match expr
		     (('head . args)
		      (just then))
		     (xs (none))))
	    *transpiler-macros*)))))

(define-transpiler-syntax (set! var expr)
  (format #f "(function()~a = ~a;\nreturn ~a\nend)()"
	  (transpile var)
	  (transpile expr)
	  (transpile var)))

(define-transpiler-syntax (lambda args expr)
  (format #f "function(~a)\n return ~a\nend"
	  (join-string "," (map convert-symbol args))
	  (transpile expr)))

(define-transpiler-syntax (define var)
  (format #f "local ~a" (convert-symbol var)))

(define-transpiler-syntax (if condition then else)
  (format #f "(function() if ~a then \nreturn ~a\nelse\nreturn ~a\nend\nend)()"
	  (transpile condition)
	  (transpile then)
	  (transpile else)))

(define-transpiler-syntax (transpiler-ffi s)
  (format #f "~a" s))

(define-syntax define-binary-operator
  (syntax-rules ()
    ((_ symbol op)
     (define-transpiler-syntax (symbol cont . args)
       (format #f "(~a)(~a)"
	       (transpile cont)
	       (join-string
		op
		(map (lambda (expr)
		       (transpile expr))
		     args)))))))

(define-binary-operator + " + ")
(define-binary-operator - " - ")
(define-binary-operator * " * ")
(define-binary-operator / " / ")

(define-syntax define-compare-operator
  (syntax-rules ()
    ((_ symbol op)
     (define-transpiler-syntax (symbol cont . args)
       (let ((exprs (map (lambda (expr) (transpile expr)) args)))
	 (format #f "(~a)(~a)"
		 (transpile cont)
		 (join-string
		  " and "
		  (map (lambda (l r) (format #f "~a ~a ~a" l op r))
		       exprs (cdr exprs)))))))))

(define-compare-operator < "<")
(define-compare-operator > ">")
(define-compare-operator <= "<=")
(define-compare-operator >= ">=")
(define-compare-operator = "==")
(define-compare-operator /= "~=")

(define (var? expr)
  (symbol? expr))

(define (convert-symbol x)
  (define (conv c)
    (cond ((eq? c #\-) "_HYPHEN_")
	  ((eq? c #\?) "_QUESTION_")
	  ((eq? c #\*) "_STAR_")
	  ((eq? c #\=) "_EQUAL_")
	  (else (list->string (list c)))))
  (list->string
   (mappend string->list
	    (map conv
		 (string->list
		  (format #f "~a" x))))))

(define (transpile-nil expr)
  (if (null? expr)
      (just (transpile 'lisp-nil))
      (none)))

(define (transpile-boolean expr)
  (if (boolean? expr)
      (just (if expr "true" "false"))
      (none)))

(define (transpile-var expr)
  (if (var? expr)
      (just (convert-symbol expr))
      (none)))

(define (transpile-number expr)
  (if (number? expr)
      (just (format #f "(~a)" expr))
      (none)))

(define (transpile-string expr)
  (if (string? expr)
      (just (format #f "\"~a\"" expr))
      (none)))

(define (transpile-macros expr)
  (find-map-just
   (lambda (f)
     (f expr))
   *transpiler-macros*))

(define (transpile-function-call expr)
  (just (format #f "(~a)(~a)"
		(transpile (car expr))
		(join-string "," (map (lambda (expr)
					(transpile expr))
				      (cdr expr))))))

(define (transpile expr)
  (get-just (find-map-just
	     (lambda (f)
	       (f expr))
	     (list
	      transpile-nil
	      transpile-boolean
	      transpile-var
	      transpile-number
	      transpile-string
	      transpile-macros
	      transpile-function-call))))

(define (read-while-eof)
  (let ((res (read)))
    (if (eof-object? res)
	()
	(cons res (read-while-eof)))))

(map (lambda (line)
       (display (transpile line))
       (display ";")
       (newline))
     (read-while-eof))
