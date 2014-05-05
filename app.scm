;;; File: app.scm

(declare (standard-bindings) (extended-bindings) (not safe))

(println "-----------------")

(println (trap-eval-string "(+ 1 2)"))

(println "-----------------")

(println (trap-eval-string "(+ (car 1) 2)"))

(println "-----------------")

(##trap
 (lambda ()
   (table-ref 111 222)))

(println "-----------------")

(define (foo x)
  (car (cons x x x)))

(##trap
  (lambda ()
    (foo 99)))

(println "-----------------")
