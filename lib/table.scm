;;;============================================================================

;;; File: "table.scm"

;;; Copyright (c) 1994-2014 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Representation of tables.

(define-type table
  id: A7AB629D-EAB0-422F-8005-08B2282E04FC
  type-exhibitor: macro-type-table
  constructor: macro-make-table
  implementer: implement-type-table
  opaque:
  macros:
  prefix: macro-

  (test  unprintable:)
  (init  unprintable:)
  (alist unprintable:)
)

(implement-type-table)

(define-fail-check-type table (macro-type-table))

(define-check-type table (macro-type-table)
  macro-table?)

(define-prim (table? obj)
  (macro-table? obj))

(define-prim (##make-table
              #!optional
              (size (macro-absent-obj))
              (init (macro-absent-obj))
              (weak-keys (macro-absent-obj))
              (weak-values (macro-absent-obj))
              (test (macro-absent-obj))
              (hash (macro-absent-obj))
              (min-load (macro-absent-obj))
              (max-load (macro-absent-obj)))

  (define (check-test arg-num)
    (if (##eq? test (macro-absent-obj))
      (checks-done ##equal?
                   arg-num)
      (let ((arg-num (##fx+ arg-num 2)))
        (macro-check-procedure
         test
         arg-num
         (make-table size: size
                     init: init
                     weak-keys: weak-keys
                     weak-values: weak-values
                     test: test
                     hash: hash
                     min-load: min-load
                     max-load: max-load)
         (checks-done test
                      arg-num)))))

  (define (checks-done test-fn arg-num)
    (macro-make-table test-fn
                      init
                      '()))

  (check-test 0))

;; TODO: implement keyword parameters
#;
(define-prim (make-table
              #!key
              (size (macro-absent-obj))
              (init (macro-absent-obj))
              (weak-keys (macro-absent-obj))
              (weak-values (macro-absent-obj))
              (test (macro-absent-obj))
              (hash (macro-absent-obj))
              (min-load (macro-absent-obj))
              (max-load (macro-absent-obj)))
  (##make-table
   size
   init
   weak-keys
   weak-values
   test
   hash
   min-load
   max-load))

(define-prim (make-table)
  (let ((size (macro-absent-obj))
        (init (macro-absent-obj))
        (weak-keys (macro-absent-obj))
        (weak-values (macro-absent-obj))
        (test (macro-absent-obj))
        (hash (macro-absent-obj))
        (min-load (macro-absent-obj))
        (max-load (macro-absent-obj)))
    (##make-table
     size
     init
     weak-keys
     weak-values
     test
     hash
     min-load
     max-load)))

(define-prim (##table-ref
              table
              key
              #!optional
              (default-value (macro-absent-obj)))

  (let ((test (macro-table-test table)))
    (let loop ((probe (macro-table-alist table)))
      (cond ((##pair? probe)
             (let ((pair (##car probe)))
               (if (test key (##car pair))
                   (##cdr pair)
                   (loop (##cdr probe)))))
            ((##not (##eq? default-value (macro-absent-obj)))
             default-value)
            ((##not (##eq? (macro-table-init table) (macro-absent-obj)))
             (macro-table-init table))
            (else
             (##raise-unbound-table-key-exception
              table-ref
              table
              key))))))

(define-prim (table-ref
              table
              key
              #!optional
              (default-value (macro-absent-obj)))
  (macro-force-vars (table key default-value)
    (macro-check-table table 1 (table-ref table key default-value)
      (##table-ref table key default-value))))
  
(define-prim (##table-set!
              table
              key
              #!optional
              (val (macro-absent-obj)))

  (let ((test (macro-table-test table))
        (alist (macro-table-alist table)))
    (let loop ((probe alist) (prev #f))

      (cond ((##pair? probe)
             (let ((pair (##car probe)))
               (if (test key (##car pair))
                   (begin
                     (if (##eq? val (macro-absent-obj))
                         (if prev
                             (##set-cdr! prev (##cdr probe))
                             (macro-table-alist-set! table (##cdr probe)))
                         (##set-cdr! pair val))
                     (##void))
                   (loop (##cdr probe) probe))))

            ((##not (##eq? val (macro-absent-obj)))
             (macro-table-alist-set!
              table
              (##cons (##cons key val) alist))
             (##void))

            (else
             (##raise-unbound-table-key-exception
              table-ref
              table
              key))))))

(define-prim (table-set!
              table
              key
              #!optional
              (val (macro-absent-obj)))
  (macro-force-vars (table key val)
    (macro-check-table table 1 (table-set! table key val)
      (##table-set! table key val))))

(define-prim (##table-length table)
  (##length (macro-table-alist table)))

(define-prim (table-length table)
  (macro-force-vars (table)
    (macro-check-table table 1 (table-length table)
      (##table-length table))))

(define-prim (##table->list table)
  (##map (lambda (x) (##cons (##car x) (##cdr x)))
         (macro-table-alist table)))

(define-prim (table->list table)
  (macro-force-vars (table)
    (macro-check-table table 1 (table->list table)
      (##table->list table))))

(define-prim (##list->table lst)
  (macro-make-table
   ##equal?
   (macro-absent-obj)
   (##map (lambda (x) (##cons (##car x) (##cdr x)))
          lst)))

(define-prim (list->table lst)
  (##list->table lst))

(define-prim (##table-copy table)
  (macro-make-table
   (macro-table-init table)
   (macro-table-test table)
   (##table->list table)))

(define-prim (table-copy table)
  (macro-force-vars (table)
    (macro-check-table table 1 (table-copy table)
      (##table-copy table))))

;;;============================================================================
