(define (##global-var->identifier x) x)
(define (##inverse-eval x) x)

;;;============================================================================

;;; Decompilation of a piece of code

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(##define-macro (mk-degen params . def)
  `(let () (##declare (not inline)) (lambda ($code ,@params) ,@def)))

(##define-macro (degen proc . args)
  `(,proc $code ,@args))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define-prim (##extract-container $code rte)
  (let loop ((c (macro-code-cte $code)) (r rte))
    (cond ((##cte-top? c)
           #f)
          ((##cte-frame? c)
           (let ((vars (##cte-frame-vars c)))
             (if (and (##pair? vars)
                      (let ((var (##car vars)))
                        (and (##var-i? var)
                             (##eq? (##var-i-name var) (macro-self-var)))))
                 (macro-rte-ref r 1)
                 (loop (##cte-parent-cte c) (macro-rte-up r)))))
          (else
           (loop (##cte-parent-cte c) r)))))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define-prim (##begin? x) (and (##pair? x) (##eq? (##car x) 'begin)))
(define-prim (##cond? x)  (and (##pair? x) (##eq? (##car x) 'cond)))
(define-prim (##and? x)   (and (##pair? x) (##eq? (##car x) 'and)))
(define-prim (##or? x)    (and (##pair? x) (##eq? (##car x) 'or)))
(define-prim (##void-constant? x)
  (and (##pair? x)
       (##eq? (##car x) 'quote)
       (##eq? (##cadr x) (##void))))

;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define-prim ##degen-top
  (mk-degen ()
    (##decomp (^ 0))))

(define-prim ##degen-cst
  (mk-degen ()
    (let ((val (^ 0)))
      (##inverse-eval val))))

(define-prim ##degen-loc-ref-x-y
  (mk-degen (up over)
    (degen ##degen-up-over up over)))

(define-prim ##degen-up-over
  (mk-degen (up over)
    (let loop1 ((c (macro-code-cte $code)) (up up))
      (cond ((##cte-frame? c)
             (if (##fx= up 0)
                 (let loop2 ((vars (##cte-frame-vars c)) (i over))
                   (if (##fx< i 2)
                       (let ((var (##car vars)))
                         (if (##var-i? var)
                             (##var-i-name var)
                             (##var-c-name var)))
                       (loop2 (##cdr vars) (##fx- i 1))))
                 (loop1 (##cte-parent-cte c) (##fx- up 1))))
            (else
             (loop1 (##cte-parent-cte c) up))))))

(define-prim ##degen-loc-ref
  (mk-degen ()
    (degen ##degen-loc-ref-x-y (^ 0) (^ 1))))

(define-prim ##degen-glo-ref
  (mk-degen ()
    (##global-var->identifier (^ 0))))

(define-prim ##degen-loc-set
  (mk-degen ()
    (##list 'set!
            (degen ##degen-up-over (^ 1) (^ 2))
            (##decomp (^ 0)))))

(define-prim ##degen-glo-set
  (mk-degen ()
    (##list 'set!
            (##global-var->identifier (^ 1))
            (##decomp (^ 0)))))

(define-prim ##degen-glo-def
  (mk-degen ()
    (##list 'define
            (##global-var->identifier (^ 1))
            (##decomp (^ 0)))))

(define-prim ##degen-if2
  (mk-degen ()
    (##list 'if
            (##decomp (^ 0))
            (##decomp (^ 1)))))

(define-prim ##degen-if3
  (mk-degen ()
    (##list 'if
            (##decomp (^ 0))
            (##decomp (^ 1))
            (##decomp (^ 2)))))

(define-prim ##degen-seq
  (mk-degen ()
    (let ((val1 (##decomp (^ 0)))
          (val2 (##decomp (^ 1))))
      (if (##begin? val2)
          (##cons 'begin (##cons val1 (##cdr val2)))
          (##list 'begin val1 val2)))))

(define-prim ##degen-quasi-list->vector
  (mk-degen ()
    (##list 'quasiquote
            (##list->vector
             (##degen-quasi-unquote-splicing-cdr
              (##decomp (^ 0)))))))

(define-prim ##degen-quasi-append
  (mk-degen ()
    (##degen-quasi-append-aux
     (##degen-quasi-unquote-splicing (##decomp (^ 0)))
     (##decomp (^ 1)))))

(define-prim ##degen-quasi-cons
  (mk-degen ()
    (##degen-quasi-append-aux
     (##degen-quasi-unquote (##decomp (^ 0)))
     (##decomp (^ 1)))))

(define-prim (##degen-quasi-append-aux a b)
  (##list 'quasiquote
          (##cons a
                  (##degen-quasi-unquote-splicing-cdr b))))

(define-prim (##degen-quasi-unquote expr)
  (let ((x (##degen-quasi-optimize expr)))
    (if x
        (##car x)
        (##list 'unquote expr))))

(define-prim (##degen-quasi-unquote-splicing-cdr expr)
  (let ((x (##degen-quasi-optimize expr)))
    (if x
        (##car x)
        (##list (##degen-quasi-unquote-splicing expr)))))

(define-prim (##degen-quasi-unquote-splicing expr)
  (##list 'unquote-splicing expr))

(define-prim (##degen-quasi-optimize expr)
  (let ((x (##degen-quasi-extract expr 'quasiquote)))
    (if x
        x
        (let ((y (##degen-quasi-extract expr 'quote)))
          (if (and y (##not (##pair? (##car y)))) ;; in case of embedded unquotes
              y
              #f)))))

(define-prim (##degen-quasi-extract expr tag)
  (and ;; #f ;; uncomment to disable optimization
   (##pair? expr)
   (##eq? (##car expr) tag)
   (let ((x (##cdr expr)))
     (and (##pair? x)
          (##null? (##cdr x))
          (##list (##car x))))))

(define-prim ##degen-cond-if
  (mk-degen ()
    (let ((val1 (##decomp (^ 0)))
          (val2 (##decomp (^ 1)))
          (val3 (##decomp (^ 2))))
      (##build-cond
       (if (##begin? val2)
           (##cons val1 (##cdr val2))
           (##list val1 val2))
       val3))))

(define-prim ##degen-cond-or
  (mk-degen ()
    (let ((val1 (##decomp (^ 0)))
          (val2 (##decomp (^ 1))))
      (##build-cond (##list val1) val2))))

(define-prim ##degen-cond-send
  (mk-degen ()
    (let ((val1 (##decomp (^ 0)))
          (val2 (##decomp (^ 1)))
          (val3 (##decomp (^ 2))))
      (##build-cond (##list val1 '=> val2) val3))))

(define-prim (##build-cond clause rest)
  (cond ((##cond? rest)
         (##cons 'cond (##cons clause (##cdr rest))))
        ((##begin? rest)
         (##cons 'cond (##list clause (##cons 'else (##cdr rest)))))
        ((##void-constant? rest)
         (##list 'cond clause))
        (else
         (##list 'cond clause (##list 'else rest)))))

(define-prim ##degen-or
  (mk-degen ()
    (let ((val1 (##decomp (^ 0)))
          (val2 (##decomp (^ 1))))
      (if (##or? val2)
          (##cons 'or (##cons val1 (##cdr val2)))
          (##list 'or val1 val2)))))

(define-prim ##degen-and
  (mk-degen ()
    (let ((val1 (##decomp (^ 0)))
          (val2 (##decomp (^ 1))))
      (if (##and? val2)
          (##cons 'and (##cons val1 (##cdr val2)))
          (##list 'and val1 val2)))))

(define-prim ##degen-case
  (mk-degen ()
    (let ((val1 (##decomp (^ 0)))
          (val2 (##decomp (^ 1))))
      (##cons 'case (##cons val1 val2)))))

(define-prim ##degen-case-clause
  (mk-degen ()
    (let ((val1 (##decomp (^ 0)))
          (val2 (##decomp (^ 1))))
      (##cons (if (##begin? val1)
                  (##cons (^ 2) (##cdr val1))
                  (##list (^ 2) val1))
              val2))))

(define-prim ##degen-case-else
  (mk-degen ()
    (let ((val (##decomp (^ 0))))
      (if (##void-constant? val)
          '()
          (##list (if (##begin? val)
                      (##cons 'else (##cdr val))
                      (##list 'else val)))))))

(define-prim ##degen-let
  (mk-degen ()
    (let ((n (macro-code-length $code)))
      (let loop ((i (##fx- n 2)) (vals '()))
        (if (##fx< 0 i)
            (loop (##fx- i 1)
                  (##cons (##decomp (macro-code-ref $code i)) vals))
            (let ((body
                   (##decomp (^ 0)))
                  (bindings
                   (##make-bindings (macro-code-ref $code (##fx- n 1))
                                    vals)))
              (if (##begin? body)
                  (##cons 'let (##cons bindings (##cdr body)))
                  (##list 'let bindings body))))))))

(define-prim (##make-bindings l1 l2)
  (if (##pair? l1)
      (##cons (##list (##car l1) (##car l2))
              (##make-bindings (##cdr l1) (##cdr l2)))
      '()))

(define-prim ##degen-letrec
  (mk-degen ()
    (let ((n (macro-code-length $code)))
      (let loop ((i (##fx- n 2)) (vals '()))
        (if (##fx< 0 i)
            (loop (##fx- i 1)
                  (##cons (##decomp (macro-code-ref $code i)) vals))
            (let ((body
                   (##decomp (^ 0)))
                  (bindings
                   (##make-bindings (macro-code-ref $code (##fx- n 1))
                                    vals)))
              (if (##begin? body)
                  (##cons 'letrec (##cons bindings (##cdr body)))
                  (##list 'letrec bindings body))))))))

(define-prim ##degen-prc-req
  (mk-degen ()
    (let* ((n (macro-code-length $code))
           (body (##decomp (^ 0)))
           (params (macro-code-ref $code (##fx- n 1))))
      (if (##begin? body)
          (##cons 'lambda (##cons params (##cdr body)))
          (##list 'lambda params body)))))

(define-prim ##degen-prc-rest
  (mk-degen ()
    (let ((body (##decomp (^ 0)))
          (params (##make-params (^ 3) #t #f '())))
      (if (##begin? body)
          (##cons 'lambda (##cons params (##cdr body)))
          (##list 'lambda params body)))))

(define-prim ##degen-prc
  (mk-degen ()
    (let ((n (macro-code-length $code)))
      (let loop ((i (##fx- n 8)) (inits '()))
        (if (##not (##fx< i 1))
            (loop (##fx- i 1)
                  (##cons (##decomp (macro-code-ref $code i)) inits))
            (let ((body
                   (##decomp (^ 0)))
                  (params
                   (##make-params
                    (macro-code-ref $code (##fx- n 1))
                    (macro-code-ref $code (##fx- n 4))
                    (macro-code-ref $code (##fx- n 3))
                    inits)))
              (if (##begin? body)
                  (##cons 'lambda (##cons params (##cdr body)))
                  (##list 'lambda params body))))))))

(define-prim (##make-params parms rest? keys inits)
  (let* ((nb-parms
          (##length parms))
         (nb-inits
          (##length inits))
         (nb-reqs
          (##fx- nb-parms (##fx+ nb-inits (if rest? 1 0))))
         (nb-opts
          (##fx- nb-inits (if keys (##vector-length keys) 0))))

    (define (build-reqs)
      (let loop ((parms parms)
                 (i nb-reqs))
        (if (##fx= i 0)
            (build-opts parms)
            (let ((parm (##car parms)))
              (##cons parm
                      (loop (##cdr parms)
                            (##fx- i 1)))))))

    (define (build-opts parms)
      (if (##fx= nb-opts 0)
          (build-rest-and-keys parms inits)
          (##cons #!optional
                  (let loop ((parms parms)
                             (i nb-opts)
                             (inits inits))
                    (if (##fx= i 0)
                        (build-rest-and-keys parms inits)
                        (let ((parm (##car parms))
                              (init (##car inits)))
                          (##cons (if (##eq? init #f) parm (##list parm init))
                                  (loop (##cdr parms)
                                        (##fx- i 1)
                                        (##cdr inits)))))))))

    (define (build-rest-and-keys parms inits)
      (if (##eq? rest? 'dsssl)
          (##cons #!rest
                  (##cons (##car parms)
                          (build-keys (##cdr parms) inits)))
          (build-keys parms inits)))

    (define (build-keys parms inits)
      (if (##not keys)
          (build-rest-at-end parms)
          (##cons #!key
                  (let loop ((parms parms)
                             (i (##vector-length keys))
                             (inits inits))
                    (if (##fx= i 0)
                        (build-rest-at-end parms)
                        (let ((parm (##car parms))
                              (init (##car inits)))
                          (##cons (if (##eq? init #f) parm (##list parm init))
                                  (loop (##cdr parms)
                                        (##fx- i 1)
                                        (##cdr inits)))))))))

    (define use-dotted-rest-parameter-when-possible? #t)

    (define (build-rest-at-end parms)
      (if (##eq? rest? #t)
          (if use-dotted-rest-parameter-when-possible?
              (##car parms)
              (##cons #!rest (##cons (##car parms) '())))
          '()))

    (build-reqs)))

(define-prim ##degen-app0
  (mk-degen ()
    (##list (##decomp (^ 0)))))

(define-prim ##degen-app1
  (mk-degen ()
    (##list (##decomp (^ 0))
            (##decomp (^ 1)))))

(define-prim ##degen-app2
  (mk-degen ()
    (##list (##decomp (^ 0))
            (##decomp (^ 1))
            (##decomp (^ 2)))))

(define-prim ##degen-app3
  (mk-degen ()
    (##list (##decomp (^ 0))
            (##decomp (^ 1))
            (##decomp (^ 2))
            (##decomp (^ 3)))))

(define-prim ##degen-app4
  (mk-degen ()
    (##list (##decomp (^ 0))
            (##decomp (^ 1))
            (##decomp (^ 2))
            (##decomp (^ 3))
            (##decomp (^ 4)))))

(define-prim ##degen-app
  (mk-degen ()
    (let ((n (macro-code-length $code)))
      (let loop ((i (##fx- n 1)) (vals '()))
        (if (##not (##fx< i 0))
            (loop (##fx- i 1)
                  (##cons (##decomp (macro-code-ref $code i)) vals))
            vals)))))

(define-prim ##degen-delay
  (mk-degen ()
    (##list 'delay (##decomp (^ 0)))))

(define-prim ##degen-future
  (mk-degen ()
    (##list 'future (##decomp (^ 0)))))

;;;----------------------------------------------------------------------------

(define ##decomp-dispatch-table #f)

(define-prim (##setup-decomp-dispatch-table)
  (set! ##decomp-dispatch-table
        (##list
         (##cons ##cprc-top         ##degen-top)

         (##cons ##cprc-cst         ##degen-cst)

         (##cons ##cprc-loc-ref-0-1 (mk-degen () (degen ##degen-loc-ref-x-y 0 1)))
         (##cons ##cprc-loc-ref-0-2 (mk-degen () (degen ##degen-loc-ref-x-y 0 2)))
         (##cons ##cprc-loc-ref-0-3 (mk-degen () (degen ##degen-loc-ref-x-y 0 3)))
         (##cons ##cprc-loc-ref-1-1 (mk-degen () (degen ##degen-loc-ref-x-y 1 1)))
         (##cons ##cprc-loc-ref-1-2 (mk-degen () (degen ##degen-loc-ref-x-y 1 2)))
         (##cons ##cprc-loc-ref-1-3 (mk-degen () (degen ##degen-loc-ref-x-y 1 3)))
         (##cons ##cprc-loc-ref-2-1 (mk-degen () (degen ##degen-loc-ref-x-y 2 1)))
         (##cons ##cprc-loc-ref-2-2 (mk-degen () (degen ##degen-loc-ref-x-y 2 2)))
         (##cons ##cprc-loc-ref-2-3 (mk-degen () (degen ##degen-loc-ref-x-y 2 3)))
         (##cons ##cprc-loc-ref     ##degen-loc-ref)
         (##cons ##cprc-loc-ref-box ##degen-loc-ref)
         (##cons ##cprc-glo-ref     ##degen-glo-ref)

         (##cons ##cprc-loc-set     ##degen-loc-set)
         (##cons ##cprc-loc-set-box ##degen-loc-set)
         (##cons ##cprc-glo-set     ##degen-glo-set)
         (##cons ##cprc-glo-def     ##degen-glo-def)

         (##cons ##cprc-if2         ##degen-if2)
         (##cons ##cprc-if3         ##degen-if3)
         (##cons ##cprc-seq         ##degen-seq)
         (##cons ##cprc-quasi-list->vector ##degen-quasi-list->vector)
         (##cons ##cprc-quasi-append ##degen-quasi-append)
         (##cons ##cprc-quasi-cons  ##degen-quasi-cons)
         (##cons ##cprc-cond-if     ##degen-cond-if)
         (##cons ##cprc-cond-or     ##degen-cond-or)
         (##cons ##cprc-cond-send-red ##degen-cond-send)
         (##cons ##cprc-cond-send-sub ##degen-cond-send)

         (##cons ##cprc-or          ##degen-or)
         (##cons ##cprc-and         ##degen-and)

         (##cons ##cprc-case        ##degen-case)
         (##cons ##cprc-case-clause ##degen-case-clause)
         (##cons ##cprc-case-else   ##degen-case-else)

         (##cons ##cprc-let         ##degen-let)
         (##cons ##cprc-letrec      ##degen-letrec)

         (##cons ##cprc-prc-req0    ##degen-prc-req)
         (##cons ##cprc-prc-req1    ##degen-prc-req)
         (##cons ##cprc-prc-req2    ##degen-prc-req)
         (##cons ##cprc-prc-req3    ##degen-prc-req)
         (##cons ##cprc-prc-req     ##degen-prc-req)
         (##cons ##cprc-prc-rest    ##degen-prc-rest)
         (##cons ##cprc-prc         ##degen-prc)

         (##cons ##cprc-app0-red    ##degen-app0)
         (##cons ##cprc-app1-red    ##degen-app1)
         (##cons ##cprc-app2-red    ##degen-app2)
         (##cons ##cprc-app3-red    ##degen-app3)
         (##cons ##cprc-app4-red    ##degen-app4)
         (##cons ##cprc-app-red     ##degen-app)
         (##cons ##cprc-app0-sub    ##degen-app0)
         (##cons ##cprc-app1-sub    ##degen-app1)
         (##cons ##cprc-app2-sub    ##degen-app2)
         (##cons ##cprc-app3-sub    ##degen-app3)
         (##cons ##cprc-app4-sub    ##degen-app4)
         (##cons ##cprc-app-sub     ##degen-app)

         (##cons ##cprc-delay       ##degen-delay)
         (##cons ##cprc-future      ##degen-future)
         )))

(##setup-decomp-dispatch-table)

;;;----------------------------------------------------------------------------

;;; Pretty-printer that decompiles procedures.


(define-prim (##decomp $code)
  (let ((cprc (macro-code-cprc $code)))
    (let ((x (##assq cprc ##decomp-dispatch-table)))
      (if x
          (degen (##cdr x))
          '?))))

