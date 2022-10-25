(transpiler-ffi "
local myprint = function(cont,...)
    print(...)
    return cont(1)
end
")

;; normal function test
(define (square x) (* x x))
(myprint "Square of 9 is " (square 9))



;; recursive function test
(define (fib n)
  (cond ((<= n 1) n)
	(else
	 (+ (fib (- n 1))
	    (fib (- n 2))))))

(myprint "The 10th fibonacchi number is " (fib 10))



;; mutual recursion test
(define (even? n)
  (if (= n 0)
      #t
      (odd? (- n 1))))

(define (odd? n)
  (if (= n 0)
      #f
      (even? (- n 1))))

(myprint "10 is even" (even? 10))
(myprint "11 is odd" (odd? 11))



;; lambda function test
((lambda (x) (myprint "x is " x)) "HELLO WORLD")
(map (lambda (x) (myprint "this is " x)) (list 1 2 3))



;; closure test
(let ((f (let ((called #f))
	   (lambda ()
	     (if (not called)
		 (begin
		   (myprint "CALLED!!!!")
		   (set! called #t)))
	     "This is result"))))
  (myprint "The result value of f is " (f))
  (myprint "The result value of f is " (f)))
