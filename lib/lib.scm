;;;============================================================================

;;; File: "lib.scm"

;;; Copyright (c) 1994-2014 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;; enable type checking for runtime library functions
(##define-macro (add-type-checks)
  '(pp ##compilation-options)
  (set! ##compilation-options (cons '(check) ##compilation-options))
  `(begin))
(add-type-checks)

(##include "../../lib/header.scm")

;; inline standard procedures
(declare (standard-bindings))

;; prevent standard inlining (which only works with C backend)
(declare (not inline-primitives equal? ##eqv?))

(##define-macro (macro-current-exception-handler)
  `(##current-exception-handler))

(define ##min-fixnum -536870912)
(define ##max-fixnum 536870911)
(define ##fixnum-width 29)
(define ##fixnum-width-neg -29)
(define ##bignum.adigit-width 14)
(define ##bignum.mdigit-width 14)
(define ##bignum.fdigit-width 14)

(##include "thread.scm")
(##include "_num.scm")
;(##include "num.scm")(##include "bignum.scm")
(##include "io.scm")
(##include "write.scm")
(##include "read.scm")
(##include "std.scm")
;(##include "eval.scm")
(##include "misc.scm")
(##include "system.scm")
(##include "table.scm")
(##include "_eval.scm")
(##include "_repl.scm")

;;;============================================================================

;; Declarations for rest of program which includes lib.scm

(declare
  (gambit-scheme)
  (separate)
  (inline)
  (inline-primitives)
  (inlining-limit 350)
  (constant-fold)
  (lambda-lift)
  (not standard-bindings)
  (not extended-bindings)
  (run-time-bindings)
  (safe)
  (interrupts-enabled)
  (proper-tail-calls)
  (not generative-lambda)
  (optimize-dead-local-variables)
  (generic)
  (mostly-fixnum-flonum)
)

;;;============================================================================
