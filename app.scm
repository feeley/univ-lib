 ;;; File: app.scm

(declare (standard-bindings) (extended-bindings) (not safe))



(begin

;(define println-old println)
;(define (println a)
;  (macro-case-target
;   ((js)
;     (##inline-host-expression 
;"(function (s) {
;  process.stdout.write(''+s);
;  return;
;}(Gambit_scm2js(Gambit_r1)))"
;"(function (s) { print(s); }(Gambit_r1))"))
;   ((c)
;   (println-old a))))

(begin
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
)
(println "-------------------------- (fib 20) -- ")
;;; Use interpreter to evaluate (fib 20)

(println
 (eval
  '(begin
     (define (fib x) (if (< x 2) 1 (+ (fib (- x 1)) (fib (- x 2)))))
     (fib 20))))



(println "----------------- (threaded-fib 20) --")
;;; Use threads to compute (fib 20), this creates 10945 threads
(define (tfib n)
  (if (< n 2)
      1
      (let ((x (make-thread (lambda () (tfib (- n 2))))))
        (thread-start! x)
        (let ((y (tfib (- n 1))))
          (+ (thread-join! x) y)))))

(println (tfib 20))



(println "----------------- continuation test --")
;;; Use continuations to implement a backtracking search for x, y, z
;;; such that x*x + y*y = z*z .

(define fail (lambda () #f))

(define (in-range a b)
  (call-with-current-continuation
   (lambda (cont)
     (enumerate a b cont))))

(define (enumerate a b cont)
  (if (< b a)
      (fail)
      (let ((save fail))
        (set! fail
          (lambda ()
            (set! fail save)
            (enumerate (+ a 1) b cont)))
        (cont a))))

(define (btsearch n)
  (set! fail (lambda () #f))
  (let* ((x (in-range 1 n))
         (y (in-range 1 n))
         (z (in-range 1 n)))
    (if (= (+ (* x x) (* y y)) (* z z))
        (begin
          (println x)
          (println y)
          (println z))
        (fail))))

(btsearch 10)


(##inline-host-statement "console.log('allo\\n');")
(##inline-host-statement "console.log(@1@);" "allo\\n")
(##inline-host-statement "console.log(Gambit_scm2js(@1@));" "allo\n")




(println "----------------- Compile scm")
(##inline-host-statement "Gambit_scm2js(@1@);"
  (lambda (a) 
     (define b 3)
     (* a b)))
(println "----------------- End compile scm")

)

;;
;; File port test.
;;

(println "----------------- Open file port")
(define a (##open-file "test"))

(println "----------------- Read file port")

(println (##readline a))
(println (##readline a))
(println (##readline a))
(println (##readline a))
(println (##readline a))

(println "----------------- Close file port")

(##close a)

(println "-----------------\n\n")


(println "---------------- Open string port")
(define a (##open-string))

(println "----------------- Write string port")

(##write-string "line 1\nline 2\nline 3\n" a)

(println "----------------- Read string port")

(println (##readline a))
(println (##readline a))
(println (##readline a))
;(println (##readline a))

(println "----------------- Close string port")

(##close a)

(println "-----------------")
