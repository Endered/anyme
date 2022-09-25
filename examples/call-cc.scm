(define cont)

(transpiler-ffi
 "
function myprint(cont,x)
  print(x)
  return 1
end
"
 )

(myprint
 (+ 1 2 3
    (call/cc (lambda (k)
	       (set! cont k)
	       (+ 10 (k 10000))))))

(cont 0)
(cont 1)
(cont 2)
