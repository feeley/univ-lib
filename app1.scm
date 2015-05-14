;;; File: app1.scm

;;;----------------------------------------------------------------------------

(declare (standard-bindings) (extended-bindings) (not safe))

(define (exception->string exc)
  (let ((port (##open-string)))
    (##display-exception exc port)
    (##newline port)
    (##get-output-string port)))

(define (handle exc)
  (println (exception->string exc)))

(println "--------------------------------")

(with-exception-catcher
 handle
 (lambda ()
   (+ 1 'foobar)))

(println "--------------------------------")

(##current-exception-handler handle)

(+ 2 'allo)

;; output:
;;
;; --------------------------------
;; (Argument 2) NUMBER expected
;; (+ 1 'foobar)
;; 
;; 
;; --------------------------------
;; (Argument 2) NUMBER expected
;; (+ 2 'allo)
;; 
;; 


(println (trap-eval-string "(+ 11 22)"))

(println (trap-eval-string "(bar 11 22)"))
