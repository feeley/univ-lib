;;; File: app1.scm

;;;----------------------------------------------------------------------------

(declare (standard-bindings) (extended-bindings) (not safe))

(println (trap-eval-string "(+ 11 22)"))

(println (trap-eval-string "(bar 11 22)"))
