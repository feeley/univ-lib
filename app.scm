;;; File: app.scm

(declare (standard-bindings) (extended-bindings))

(##inline-host-declaration #<<host-code-end

function jsfn(f) {
  print("jsfn start");
  f("hello!"); // f is either the Scheme handler1 or handler2 functions
  print("jsfn end");
}

host-code-end
)

(define jsfn (##inline-host-expression "Gambit_js2scm(jsfn)"))

(define (handler1 x)
  (println "handler1 start")
  (println x)
  (println "handler1 end"))

(define (handler2 x)
  (##trap
   (lambda ()
     (println "handler2 start")
     (foo x)
     (println "handler2 end"))))

(println "--------------")
(jsfn handler1)
(println "--------------")
(jsfn handler2)
(println "--------------")
