(define (read-while-eof)
  (let ((res (read)))
    (if (eof-object? res)
	()
	(cons res (read-while-eof)))))


(define headers
  '(
    (define (array-length arr)
      (cps-call table.maxn arr))
    (define (array->list arr)
      (define (rec i acc)
	(if (= i 0)
	    acc
	    (rec (- i 1)
		 (cons (ref arr i) acc))))
      (rec (array-length arr) ()))))


(map (lambda (line)
       (write line)
       (newline))
     (append headers
	     (read-while-eof)))

(procedure? identity)
