(define (read-while-eof)
  (let ((res (read)))
    (if (eof-object? res)
	()
	(cons res (read-while-eof)))))


(define headers
  '(
    (define lisp-nil
      (lambda (x)
	"lisp-nil"))
    (define (cons car cdr)
      (lambda (x)
	(cond ((= x "lisp-car") car)
	      ((= x "lisp-cdr") cdr)
	      ((= x "lisp-type") "lisp-cons"))))
    (define (car c)
      (c "lisp-car"))
    (define (cdr c)
      (c "lisp-cdr"))
    (define (null? x)
      (and (procedure? x)
	   (= (x "lisp-type") "lisp-nil")))
    (define (pair? x)
      (and (procedure? x)
	   (= (x "lisp-type") "lisp-cons")))))


(map (lambda (line)
       (write line)
       (newline))
     (append headers
	     (read-while-eof)))

(procedure? identity)
