;;; File: app6.scm

;;;----------------------------------------------------------------------------

(declare (standard-bindings) (extended-bindings) (not safe))
(declare (not inline-primitives equal? ##eqv?))

;;;============================================================================

(define make-adder
  (lambda (x)
    (if x
        (lambda (y) (##fx+ x y))
        (lambda (z) (set! x z)))))

(define foo (make-adder 42))

(define (serdes obj)
  (let* ((s (object->u8vector obj))
         (d (u8vector->object s)))
    d))

(define (check-eq? obj)
  (if (not (##eq? obj (serdes obj))) (println "error " obj)))

(define (check-equal? obj)
  (if (not (equal? obj (serdes obj))) (println "error " obj)))

(check-eq?  #f)
(check-eq?  #t)
(check-eq?  '())
(check-eq?  #!eof)
(check-eq?  #!void)
(check-eq?  (macro-absent-obj))
(check-eq?  #!unbound)
(check-eq?  #!unbound2)
(check-eq?  #!optional)
(check-eq?  #!key)
(check-eq?  #!rest)
(macro-case-target ((c C) (check-eq?  (macro-unused-obj))))
(macro-case-target ((c C) (check-eq?  (macro-deleted-obj))))

(check-equal? (cons 11 22))
(check-equal? -123)
(check-equal? 123)
(check-equal? 0)
(check-equal? 10)
(check-equal? 11)
(check-equal? 128)

(check-eq? 'allo)
(check-eq? 'allo:)

(check-equal? "hello")
(check-equal? (vector 11 22 33))
;(check-equal? (file-info "."))
(check-eq? car)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;(check-equal? 1.25)
;;(check-equal? 1234567890123456789)
;(check-equal? 1/3)
;(check-equal? 1+3i)

#|
(check-equal? (box 11))
;(check-equal? (values 1 2 3))

(check-equal? (u8vector 1 2 3))
(check-equal? (u16vector 1 2 3))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;(check-equal? (f64vector 1.1 2.2 3.3))
|#

#;
(let ((o (file-info ".")))
  (pretty-print o)
  (let ((so (object->u8vector o)))
    (let ((do (u8vector->object so)))
      (pretty-print do))))

#;
(let ((p (delay (println "*******"))))
  (let ((sp (object->u8vector p)))
    (let ((dp (u8vector->object sp)))
      (println "------")
      (force dp)
      (force dp))))

(check-eq? #\X)


(pretty-print foo)

(define sfoo (object->u8vector foo))

(pretty-print sfoo)

(define dfoo (u8vector->object sfoo))

(pretty-print dfoo)

(pretty-print (dfoo 10))

(pretty-print (list->string (map integer->char (u8vector->list sfoo))))

(println "-----------------")

(define (test-cont)
  
(define (showcc result)
  (##continuation-capture
   (lambda (c)
     (let ((sc (object->u8vector c)))
       ;;(write sc ##stdout-port)
       ;;(newline ##stdout-port)
       (let ((dc (u8vector->object sc)))
         (##continuation-return-no-winding dc result))))))

(define first #t)

(define (fib n)
  (if (< n 2)
      (if first
          (begin
            (set! first #f)
            (showcc n))
          n)
      (+ (fib (- n 1))
         (fib (- n 2)))))

;(pretty-print (fib 20))

(define (ev n)
  (if (= n 0)
      (showcc #t)
      (if (ev (- n 1))
          #f
          #t)))

(list
(showcc 123)
(ev 10)
(let ((a (u8vector-length sfoo))) (+ (showcc 123) a))
(fib 3)
)
)

(pretty-print (test-cont))
