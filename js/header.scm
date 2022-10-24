(define (read-while-eof)
  (let ((res (read)))
    (if (eof-object? res)
	()
	(cons res (read-while-eof)))))


(define headers
  '(
    (transpiler-ffi-header "
let array_empty = (cont) => {
    return cont([])
}
let is_procedure = (cont,x) => {
    return cont(typeof(x) == 'function')
}
let array_length = (cont,x) => {
    return cont(x.length)
}
let array_push = (cont,x,v) => {
    return cont(x.push(v))
}

let lisp_engine = (v) => {
    while(v(\"lisp-type\") == \"lisp-continuation\"){
        v = v(\"lisp-continue\")
    }
    if(v(\"lisp-type\") != \"lisp-result\")console.log(\"lisp-type is invalid\")
    return v(\"lisp-get-result\")
}

let lisp_result = (v) => {
    return (x) => {
        if(x == 'lisp-type')return 'lisp-result'
        if(x == 'lisp-get-result')return v
        console.log('i dont know that argument')
    }
}

let lisp_continuation = (f) => {
    return (x) => {
        if(x == 'lisp-type')return 'lisp-continuation'
        if(x == 'lisp-continue')return f()
        console.log('i dont know that argument')
    }
}
")
    (define (array-push x v)
      (array_push x v))
    (define (array-length x)
      (array_length x))
    (define (array->list arr)
      (define (rec i acc)
	(if (= i -1)
	    acc
	    (rec (- i 1)
		 (cons (ref arr i) acc))))
      (rec (- (array_length arr) 1) ()))
    (define (list->array lst)
      (let ((res (array_empty)))
	(map1 (lambda (x)
		(array_push res x))
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
