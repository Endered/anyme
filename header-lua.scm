(define (read-while-eof)
  (let ((res (read)))
    (if (eof-object? res)
	()
	(cons res (read-while-eof)))))


(define headers
  '(
    (transpiler-ffi "
local array_empty = function(cont)
    return cont({})
end
local is_procedure = function(cont,x)
    return cont(type(x) == 'function')
end
")
    (define (array-length arr)
      (cps-call table.maxn arr))
    (define (array->list arr)
      (define (rec i acc)
	(if (= i 0)
	    acc
	    (rec (- i 1)
		 (cons (ref arr i) acc))))
      (rec (array-length arr) ()))
    (define (list->array lst)
      (let ((res (array_empty)))
	(map1 (lambda (x)
		(cps-call table.insert res x))
	      lst)
	res))
    (define (procedure? x)
      (is_procedure x))
    ))


(map (lambda (line)
       (write line)
       (newline))
     (append headers
	     (read-while-eof)))

(procedure? identity)
