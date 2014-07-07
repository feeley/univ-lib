;;;============================================================================

;;; File: "misc.scm"

;;; Copyright (c) 2014 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;; More Scheme runtime library procedures.

(define-prim (apply f lst)
  (##apply f lst))

(define (##check-heap-limit)
  #f)

(define-prim (##eq? x y))

(define (##set-debug-settings! . rest) #f)
(define (##fxarithmetic-shift-right . rest) 0)

(define (##closure? x) #t)
(define (##global-var->identifier x) x)

(define (continuation-capture proc)
  (##continuation-capture proc))

(define (continuation-return cont val)
  (##continuation-return-no-winding cont val))

(define (##continuation-ret k)
  (##frame-ret (##continuation-frame k)))

(define (##subprocedure-parent proc) proc)
(define (##subprocedure-id proc) 0)
(define (##subprocedure-parent-info proc) #f)

(define (##output-port-width port) 80)
(define (##output-port-column port) 0)

(define (##subproc-id subproc)
  (macro-case-target
   ((js)
    (##string->symbol
     (##inline-host-expression "gambit_js2scm(@1@.id)" subproc)))
   (else
    'unknown)))

(define (##object->global-var->identifier obj)
  (macro-case-target
   ((js)
    (let ((x (##inline-host-expression "(function (obj) {
  for (id in gambit_glo) {
    if (gambit_glo[id] === obj) {
      return gambit_js2scm(id);
    }
  }
  return gambit_js2scm(false);
}(@1@))" obj)))
      (and x (##string->symbol x))))
   (else
    (and x 'unknown))))

(define (##object->string obj #!optional (width #f))
  (generic-write-to-string obj #f width))

;;; Structure support.

(##define-macro (type-type-literal) (list 'quote ##type-type))
(define ##type-type (type-type-literal))

(define-prim (##type-id type)
  (##unchecked-structure-ref type 1 ##type-type ##type-id))

(define-prim (##type-name type)
  (##unchecked-structure-ref type 2 ##type-type ##type-name))

(define-prim (##type-flags type)
  (##unchecked-structure-ref type 3 ##type-type ##type-flags))

(define-prim (##type-super type)
  (##unchecked-structure-ref type 4 ##type-type ##type-super))

(define-prim (##type-fields type)
  (##unchecked-structure-ref type 5 ##type-type ##type-fields))

(define-prim (##unchecked-structure-ref obj i type proc))

(define-prim (##unchecked-structure-set! obj val i type proc))


#;
(define-prim (##structure-direct-instance-of? obj type-id)
  (and (##structure? obj)
       (##eq? (##type-id (##structure-type obj))
              type-id)))

#;
(define-prim (##structure-instance-of? obj type-id)
  (and (##structure? obj)
       (let loop ((c (##structure-type obj)))
         (if (##eq? (##type-id c) type-id)
           #t
           (let ((super (##type-super c)))
             (and super
                  (loop super)))))))

(define-prim (##type? obj)
  (##structure-direct-instance-of? obj (##type-id ##type-type)))

(define (##display-cont-backtrace cont port)
  (let loop ((i 0) (cont cont))
    (if (##continuation-frame cont)
        (let ((id (##subproc-id (##continuation-ret cont))))
          (##write i port)
          (##write-string " " port)
          (##write id port)
          (##newline port)
          (loop (##fx+ i 1) (##continuation-next cont))))))

(define (##trap thunk)
  (##continuation-capture
   (lambda (return)
     (with-exception-handler
      (lambda (exc)
        (##continuation-capture
         (lambda (cont)
           (##continuation-graft-no-winding
            return
            (lambda ()
              (let ((port (##open-string)))
                (##write-string "*** ERROR: " port)
                (##display-exception exc port)
                (##newline port)
                (##display-cont-backtrace cont port)
                (println (##get-output-string port))
                (##void)))))))
      thunk))))

(define (trap-eval-string str)
  (##trap (lambda () (eval-string str))))

(define (eval-string str)
  (object->string (eval (string->object str))))

;;;============================================================================

(implement-library-type-nonprocedure-operator-exception)

(define-prim (##apply-global-with-procedure-check-nary gv . args)
  (##declare (not interrupts-enabled))
  #;(##apply-with-procedure-check (##global-var-ref gv) args)
  ;; TODO: for now this gives better error messages because the backtrace lacks information
  (##apply-with-procedure-check gv args))

(define-prim (##apply-with-procedure-check-nary oper . args)
  (##declare (not interrupts-enabled))
  (##apply-with-procedure-check oper args))

(define-prim (##apply-with-procedure-check oper args)
  (##declare (not interrupts-enabled))
  (macro-force-vars (oper)
    (if (##procedure? oper)
      (##apply oper args)
      (##raise-nonprocedure-operator-exception oper args #f #f))))

(define-prim (##raise-nonprocedure-operator-exception oper args code rte)
  (##declare (not interrupts-enabled))
  (macro-raise
   (macro-make-nonprocedure-operator-exception oper args code rte)))

(implement-library-type-wrong-number-of-arguments-exception)

(define-prim (##raise-wrong-number-of-arguments-exception-nary proc . args)
  (##declare (not interrupts-enabled))
  (##raise-wrong-number-of-arguments-exception proc args))

(define-prim (##raise-wrong-number-of-arguments-exception proc args)
  (##declare (not interrupts-enabled))
  (macro-raise
   (macro-make-wrong-number-of-arguments-exception proc args)))

(implement-library-type-keyword-expected-exception)

(define-prim (##raise-keyword-expected-exception-nary proc . args)
  (##declare (not interrupts-enabled))
  (##raise-keyword-expected-exception proc args))

(define-prim (##raise-keyword-expected-exception proc args)
  (##declare (not interrupts-enabled))
  (macro-raise
   (macro-make-keyword-expected-exception proc args)))

(implement-library-type-unknown-keyword-argument-exception)

(define-prim (##raise-unknown-keyword-argument-exception-nary proc . args)
  (##declare (not interrupts-enabled))
  (##raise-unknown-keyword-argument-exception proc args))

(define-prim (##raise-unknown-keyword-argument-exception proc args)
  (##declare (not interrupts-enabled))
  (macro-raise
   (macro-make-unknown-keyword-argument-exception proc args)))

(implement-library-type-type-exception)

(define-prim (##raise-type-exception arg-num type-id proc args)
  (##extract-procedure-and-arguments
   proc
   args
   arg-num
   type-id
   #f
   (lambda (procedure arguments arg-num type-id dummy)
     (macro-raise
      (macro-make-type-exception procedure arguments arg-num type-id)))))

(define-prim (##argument-list-remove-absent! lst tail)
  (let loop ((lst1 tail)
             (lst2 #f)
             (lst3 lst))
    (if (##pair? lst3)
      (let ((val (##car lst3)))
        (if (##eq? val (macro-absent-obj))
          (loop lst1
                lst2
                (##cdr lst3))
          (loop (if lst2
                  (begin
                    (##set-cdr! lst2 lst3)
                    lst1)
                  lst3)
                lst3
                (##cdr lst3))))
      (begin
        (if lst2
          (##set-cdr! lst2 tail))
        lst1))))

(define-prim (##argument-list-remove-absent-keys! lst)
  (let loop ((lst1 #f)
             (lst2 #f)
             (lst3 lst))
    (if (and (##pair? lst3) (##keyword? (##car lst3)))
      (let ((val (##cadr lst3)))
        (if (##eq? val (macro-absent-obj))
          (loop lst1
                lst2
                (##cddr lst3))
          (loop (if lst2
                  (begin
                    (##set-cdr! lst2 lst3)
                    lst1)
                  lst3)
                (##cdr lst3)
                (##cddr lst3))))
      (let ((tail (if (##pair? lst3) (##car lst3) '())))
        (if lst2
          (begin
            (##set-cdr! lst2 tail)
            lst1)
          tail)))))

(define-prim (##argument-list-fix-rest-param! lst)
  (let loop ((curr #f) (next lst))
    (let ((tail (##cdr next)))
      (if (##pair? tail)
        (loop next tail)
        (if curr
          (begin
            (##set-cdr! curr (##car next))
            lst)
          (##car next))))))

(define-prim (##extract-procedure-and-arguments proc args val1 val2 val3 cont)
  (cond ((##null? proc)
         (cont (##car args)
               (##argument-list-remove-absent!
                (##argument-list-fix-rest-param! (##cdr args))
                '())
               val1
               val2
               val3))
        ((##pair? proc)
         (cont (##car proc)
               (##argument-list-remove-absent!
                args
                (##argument-list-remove-absent-keys! (##cdr proc)))
               val1
               val2
               val3))
        (else
         (cont proc
               (##argument-list-remove-absent! args '())
               val1
               val2
               val3))))

;;;============================================================================
