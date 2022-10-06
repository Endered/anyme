(define (read-while-eof)
  (let ((res (read)))
    (if (eof-object? res)
	()
	(cons res (read-while-eof)))))


(define headers
  '(
    (transpiler-ffi-header "
local array_empty = function(cont)
    return cont({})
end

local is_procedure = function(cont,x)
    return cont(type(x) == 'function')
end

local lisp_engine = function(v)
    while v(\"lisp-type\") == \"lisp-continuation\" do
        v = v(\"lisp-continue\")
    end
    if not v(\"lisp-type\") == \"lisp-result\" then
        print(\"lisp-type is invalid\")
    end
    return v(\"lisp-get-result\")
end

local lisp_result = function(v) 
    return function(x)
        if x == 'lisp-type' then return 'lisp-result' end
        if x == 'lisp-get-result' then return v end
        print('i dont know that argument')
    end
end

local lisp_continuation = function(f)
    return function(x)
        if x == 'lisp-type' then return 'lisp-continuation' end
        if x == 'lisp-continue' then return f() end
        print('i dont know that argument')
    end
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
