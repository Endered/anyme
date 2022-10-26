(define (myprint x)
  (cps-call print x))

(defmacro (define-two-function)
  `(begin
     (define (f)
       (myprint "Hello. This is f"))
     (define (g)
       (myprint "Yeaaaaaaaaaaaaaaaaaaaaah!"))))

(define-two-function)

(f)
(g)
