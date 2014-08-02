;;;============================================================================

;;; File: "thread.scm"

;;; Copyright (c) 2014 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;; Thread related data types

(define-type thread-base
  id: 70122FEB-A7FE-4F62-A1C1-D46D3373DAAB
  extender: define-type-of-thread-base
  macros:
  prefix: macro-

  btq-next
  btq-prev
  toq-next
  toq-prev
)

(define-type-of-thread-base thread
  id: 4FCDC6AC-0DF9-485C-A67C-1CD3F30917CA
  macros:
  prefix: macro-
  type-exhibitor: macro-type-thread

  cont
  denv
  state
  thunk
  result
  mutex
  condvar
  name
)

(define-type-of-thread-base mutex
  id: 39947926-EB9E-4B06-BA88-FF2C3E1A0E83
  macros:
  prefix: macro-

  owner
  name
)

(define-type-of-thread-base condvar
  id: BB0739DC-AAE0-42C6-AD19-1787DF2CA329
  macros:
  prefix: macro-

  name
)

;; Blocked thread queue operations.

(define-macro (macro-btq-init! t)
  `(let ((t ,t))
     (macro-thread-base-btq-next-set! t t)
     (macro-thread-base-btq-prev-set! t t)
     t))

(define-macro (macro-btq-add! t q)
  `(let ((t ,t) (q ,q))
     (let ((p (macro-thread-base-btq-prev q)))
       (macro-thread-base-btq-prev-set! t p)
       (macro-thread-base-btq-next-set! p t)
       (macro-thread-base-btq-prev-set! q t)
       (macro-thread-base-btq-next-set! t q)
       t)))

(define-macro (macro-btq-remove! t)
  `(let ((t ,t))
     (let ((n (macro-thread-base-btq-next t))
           (p (macro-thread-base-btq-prev t)))
       (macro-thread-base-btq-prev-set! n p)
       (macro-thread-base-btq-next-set! p n)
       t)))

;; Timeout queue operations.

(define-macro (macro-toq-init! t)
  `(let ((t ,t))
     (macro-thread-base-toq-next-set! t t)
     (macro-thread-base-toq-prev-set! t t)
     t))

(define-macro (macro-toq-add! t q)
  `(let ((t ,t) (q ,q))
     (let ((p (macro-thread-base-toq-prev q)))
       (macro-thread-base-toq-prev-set! t p)
       (macro-thread-base-toq-next-set! p t)
       (macro-thread-base-toq-prev-set! q t)
       (macro-thread-base-toq-next-set! t q)
       t)))

(define-macro (macro-toq-remove! t)
  `(let ((t ,t))
     (let ((n (macro-thread-base-toq-next t))
           (p (macro-thread-base-toq-prev t)))
       (macro-thread-base-toq-prev-set! n p)
       (macro-thread-base-toq-next-set! p n)
       t)))

;; Create an empty ready queue

(define ##ready-queue
  (macro-btq-init!
   (macro-toq-init!
    (macro-make-thread-base #f #f #f #f))))

(define-macro (macro-make-runnable! thread)
  `(macro-btq-add! ,thread ##ready-queue))

;; Create the primordial continuation used for new threads

(define ##primordial-continuation #f)
(define ##initial-continuation #f)

(##continuation-capture
 (lambda (resume)
   (set! ##primordial-continuation (##continuation-next resume))
   (##continuation-graft-no-winding
    ##primordial-continuation
    (lambda ()
      (##continuation-capture
       (lambda (cont)
         (set! ##initial-continuation cont)
         (##continuation-return-no-winding resume #f)))
      (let* ((ct (##current-thread))
             (thunk (macro-thread-thunk ct)))
        ;;(println (list (macro-thread-name ct) " starting"));;;;;;;;;;;;
        (macro-thread-state-set! ct #f)
        ;;(println thunk)
        (let ((result (thunk)))
          (##thread-end! 'terminated-normally result)))))))

;; Thread scheduler

(define (##thread-end-with-uncaught-exception! exc)
  (##thread-end! 'uncaught-exception exc))

(define (##thread-end! state result)
  (let ((ct (##current-thread)))

    (##mutex-lock! (macro-thread-mutex ct))

    (macro-thread-state-set! ct state)
    (macro-thread-result-set! ct result)

    (##condvar-broadcast! (macro-thread-condvar ct))

    (##mutex-unlock! (macro-thread-mutex ct))

    (macro-btq-init! (macro-btq-remove! ct))
    (##thread-schedule!)))

(define (##thread-schedule!)
  ;;(println "##thread-schedule!")
  ;;(dump-rq)
  ;;(println "........")
  (let ((t (macro-thread-base-btq-next ##ready-queue)))
    (if (##eq? t ##ready-queue)

        (##continuation-return-no-winding ;; terminate program
         ##primordial-continuation
         #f)

        (##thread-restore!
         t
         (lambda ()
           ;;(println (list (macro-thread-name t) " restoring"));;;;;;;;;;;;
           #f)))))

(define (dump-rq)
  (let loop1 ((x ##ready-queue))
    (let ((n (macro-thread-base-btq-next x)))
      (if (##not (##eq? n ##ready-queue))
          (begin (println (macro-thread-name n)) (loop1 n)))))
  (let loop2 ((x ##ready-queue))
    (let ((p (macro-thread-base-btq-prev x)))
      (if (##not (##eq? p ##ready-queue))
          (begin (println (macro-thread-name p)) (loop2 p))))))

;; Thread operations

(define (##make-thread thunk name)
  (declare (not interrupts-enabled))
  (macro-btq-init!
   (macro-toq-init!
    (macro-make-thread #f ;; btq-next
                       #f ;; btq-prev
                       #f ;; toq-next
                       #f ;; toq-prev
                       ##initial-continuation ;; cont
                       '() ;; denv
                       'new ;; state
                       thunk ;; thunk
                       #f ;; result
                       (##make-mutex (##void)) ;; mutex
                       (##make-condvar (##void)) ;; condvar
                       name ;; name
                      ))))

(define-prim (make-thread
              thunk
              #!optional
              (n (macro-absent-obj)))
  (macro-force-vars (thunk n tg)
    (let* ((name
            (if (##eq? n (macro-absent-obj))
              (##void)
              n)))
      (macro-check-procedure thunk 1 (make-thread thunk n tg)
        (##make-thread thunk name)))))

(define (##thread-start! t)
  (declare (not interrupts-enabled))
  (macro-thread-state-set! t #f)
  (macro-make-runnable! t)
  t)

(define (thread-name t)
  (macro-thread-name t))

(define (thread-start! t)
  (##thread-start! t))

(define (##thread-join! t)
  (let loop ()
    (##mutex-lock! (macro-thread-mutex t))

    (case (macro-thread-state t)

      ((terminated-normally)
       (let ((result (macro-thread-result t)))
         (##mutex-unlock! (macro-thread-mutex t))
         result))

      ((uncaught-exception)
       (let ((exc (macro-thread-result t)))
         (##mutex-unlock! (macro-thread-mutex t))
         (##raise exc)))

      (else
       (##mutex-unlock! (macro-thread-mutex t) (macro-thread-condvar t))
       (loop)))))

(define (thread-join! t)
  (##thread-join! t))

(define (##thread-yield!)
  (declare (not interrupts-enabled))
  ;;(println "##thread-yield!")
  (let ((ct (##current-thread)))
    (if (##eq? ct (macro-thread-base-btq-prev ##ready-queue))
        #f ;; only one runnable thread, so don't context switch
        (##thread-save!
         (lambda (ct)
           ;;(println (list (macro-thread-name ct) " saving"));;;;;;;;;;;;
           (macro-make-runnable! (macro-btq-remove! ct))
           (##thread-schedule!))))))

(define (thread-yield!)
  (##thread-yield!))

;; Mutex operations

(define (##make-mutex name)
  (declare (not interrupts-enabled))
  (macro-btq-init!
   (macro-make-mutex #f ;; btq-next
                     #f ;; btq-prev
                     #f ;; toq-next
                     #f ;; toq-prev
                     #f ;; owner
                     name ;; name
                     )))

(define-prim (make-mutex #!optional (n (macro-absent-obj)))
  (macro-force-vars (n)
    (let ((name
           (if (##eq? n (macro-absent-obj))
             (##void)
             n)))
      (##make-mutex name))))

(define-prim (mutex-name mutex)
  (macro-force-vars (mutex)
    (macro-mutex-name mutex)))

(define (##mutex-lock! mut)
  (declare (not interrupts-enabled))
  (let ((owner (macro-mutex-owner mut)))
    (if owner
        (##thread-save!
         (lambda (ct)
           (macro-btq-add! (macro-btq-remove! ct) mut)
           (##thread-schedule!)))
        (macro-mutex-owner-set! mut (##current-thread)))
    #t))

(define (mutex-lock! mut)
  (##mutex-lock! mut))

(define (##mutex-unlock! mut #!optional condvar)
  (declare (not interrupts-enabled))

  (define (done new-owner)
    (macro-mutex-owner-set! mut new-owner)
    (if condvar
        (##thread-save!
         (lambda (ct)
           (macro-btq-add! (macro-btq-remove! ct) condvar)
           (##thread-schedule!))))
    #t)
        
  (let ((owner (macro-mutex-owner mut)))
    (if owner
        (let ((next (macro-thread-base-btq-next mut)))
          (if (##not (##eq? next mut)) ;; thread blocked on mutex?
              (begin
                (macro-make-runnable! (macro-btq-remove! next))
                (done next))
              (done #f)))
        (done #f))))

(define (mutex-unlock! mut #!optional condvar)
  (##mutex-unlock! mut condvar))

;; Condition variable operations

(define (##make-condvar name)
  (declare (not interrupts-enabled))
  (macro-btq-init!
   (macro-make-condvar #f ;; btq-next
                       #f ;; btq-prev
                       #f ;; toq-next
                       #f ;; toq-prev
                       name ;; name
                       )))

(define-prim (make-condition-variable #!optional (n (macro-absent-obj)))
  (macro-force-vars (n)
    (let ((name
           (if (##eq? n (macro-absent-obj))
             (##void)
             n)))
      (##make-condvar name))))

(define-prim (condition-variable-name condvar)
  (macro-force-vars (condvar)
    (macro-condvar-name condvar)))

(define (##condvar-signal! condvar)
  (declare (not interrupts-enabled))
  (let ((next (macro-thread-base-btq-next condvar)))
    (if (##not (##eq? next condvar))
          (begin
            (macro-make-runnable! (macro-btq-remove! next))
            (##void))
          (##void))))

(define (condition-variable-signal! condvar)
  (##condvar-signal! condvar))

(define (##condvar-broadcast! condvar)
  (declare (not interrupts-enabled))
  (let loop ()
    (let ((next (macro-thread-base-btq-next condvar)))
      (if (##not (##eq? next condvar))
          (begin
            (macro-make-runnable! (macro-btq-remove! next))
            (loop))
          (##void)))))

(define (condition-variable-broadcast! condvar)
  (##condvar-broadcast! condvar))

;;;----------------------------------------------------------------------------

;;; Parameter objects.

(##define-macro (macro-make-parameter-descr value filter)
  `(##vector ,value ,filter))

(##define-macro (macro-parameter-descr-value p)         `(macro-slot 0 ,p))
(##define-macro (macro-parameter-descr-value-set! p x)  `(macro-slot 0 ,p ,x))
(##define-macro (macro-parameter-descr-filter p)        `(macro-slot 1 ,p))
(##define-macro (macro-parameter-descr-filter-set! p x) `(macro-slot 1 ,p ,x))

(define-prim (##make-parameter
              init
              #!optional
              (f (macro-absent-obj)))
  (let ((filter
         (if (##eq? f (macro-absent-obj))
             (lambda (x) x)
             f)))
    (macro-check-procedure filter 2 (make-parameter init f)
      (let ((descr
             (macro-make-parameter-descr (filter init) filter)))
        (letrec ((param
                  (lambda (#!optional (new-val (macro-absent-obj)))
                    (if (##eq? new-val (macro-absent-obj))
                        (##dynamic-ref param)
                        (##dynamic-set!
                         param
                         ((macro-parameter-descr-filter descr) new-val))))))
          param)))))

(define-prim (make-parameter init #!optional (f (macro-absent-obj)))
  (macro-force-vars (f)
    (##make-parameter init f)))

(define-prim (##parameter? obj)
  (##declare (not interrupts-enabled))
  (and (##procedure? obj)
;;;;;       (##closure? obj)
       (##eq? (##closure-code obj)
              (##closure-code ##current-exception-handler))))

(##define-macro (macro-parameter-descr param)
  `(##closure-ref ,param 1))

(define-prim (##parameterize param val thunk)
  (##declare (not interrupts-enabled))
  (macro-check-procedure param 1 (##parameterize param val thunk)
    (macro-check-procedure thunk 3 (##parameterize param val thunk)
      (if (##parameter? param)
          (##dynamic-let
           param
           ((macro-parameter-descr-filter (macro-parameter-descr param)) val)
           thunk)
          (let ((save (param)))
            (##dynamic-wind
             (lambda () ;; before
               (param val))
             thunk
             (lambda () ;; after
               (param save))))))))

(define-prim (##dynamic-ref param)
  (let ((descr (macro-parameter-descr param)))
    (let ((x (##assq descr (macro-thread-denv (##current-thread)))))
      (if x
          (##cdr x)
          (macro-parameter-descr-value descr)))))

(define-prim (##dynamic-set! param val)
  (let ((descr (macro-parameter-descr param)))
    (let ((x (##assq descr (macro-thread-denv (##current-thread)))))
      (if x
          (##set-cdr! x val)
          (macro-parameter-descr-value-set! descr val)))))

(define-prim (##dynamic-let param val thunk)
  (##dynamic-env-bind
   (##cons (##cons (macro-parameter-descr param) val)
           (macro-thread-denv (##current-thread)))
   thunk))

(define-prim (##dynamic-env-bind denv thunk)
  (##declare (not interrupts-enabled))
  (let* ((ct (##current-thread))
         (save (macro-thread-denv ct)))
    (macro-thread-denv-set! ct denv)
    (let ((results (thunk)))
      (macro-thread-denv-set! (##current-thread) save)
      results)))

(define-prim (##assq obj lst)
  (let loop ((x lst))
    (if (##pair? x)
        (let ((couple (##car x)))
          (if (##eq? obj (##car couple))
              couple
              (loop (##cdr x))))
        #f)))

;;;----------------------------------------------------------------------------

;; User accessible primitives for exception handling.

(define-prim (with-exception-handler handler thunk)
  (macro-force-vars (handler thunk)
    (macro-check-procedure handler 1 (with-exception-handler handler thunk)
      (macro-check-procedure thunk 2 (with-exception-handler handler thunk)
        (##dynamic-let
         ##current-exception-handler
         handler
         thunk)))))

(define-prim (##with-exception-catcher catcher thunk)
  (##continuation-capture
   (lambda (cont)
     (##dynamic-let
      ##current-exception-handler
      (lambda (exc)
;;        (##continuation-graft cont catcher exc))
        (##continuation-graft-no-winding cont catcher exc))
      thunk))))

(define-prim (with-exception-catcher catcher thunk)
  (macro-force-vars (catcher thunk)
    (macro-check-procedure catcher 1 (with-exception-catcher catcher thunk)
      (macro-check-procedure thunk 2 (with-exception-catcher catcher thunk)
        (##with-exception-catcher catcher thunk)))))

(define ##current-exception-handler
  (##make-parameter
   (lambda (exc)
     (##thread-end-with-uncaught-exception! exc))
   (lambda (val)
     (macro-check-procedure val 1 (##current-exception-handler val)
       val))))

(define current-exception-handler
  ##current-exception-handler)

(define-prim (##raise obj)
  (macro-raise obj))

(define-prim (raise obj)
  (macro-raise obj))

(define-prim (##abort obj)
  (macro-abort obj))

(define-prim (abort obj)
  (macro-abort obj))

;;;----------------------------------------------------------------------------

(define-prim (##call-with-current-continuation
              receiver
              #!optional
              (lift1 (macro-absent-obj))
              (lift2 (macro-absent-obj))
              (lift3 (macro-absent-obj))
              #!rest
              others)

  (define (reify-continuation cont)
    (lambda (val)
      (##continuation-return-no-winding cont val)))

  (cond ((##eq? lift1 (macro-absent-obj))
         (##continuation-capture
          (lambda (cont)
            (receiver (reify-continuation cont)))))
        ((##eq? lift2 (macro-absent-obj))
         (##continuation-capture
          (lambda (cont lift1)
            (receiver (reify-continuation cont) lift1))
          lift1))
        ((##eq? lift3 (macro-absent-obj))
         (##continuation-capture
          (lambda (cont lift1 lift2)
            (receiver (reify-continuation cont) lift1 lift2))
          lift1
          lift2))
        ((##null? others)
         (##continuation-capture
          (lambda (cont lift1 lift2 lift3)
            (receiver (reify-continuation cont) lift1 lift2 lift3))
          lift1
          lift2
          lift3))
        (else
         (let ((lifts
                (##cons lift1
                        (##cons lift2
                                (##cons lift3
                                        others)))))
           (##continuation-capture
            (lambda (cont)
              (##apply
               receiver
               (##cons (reify-continuation cont) lifts))))))))

(define-prim (call-with-current-continuation
              receiver
              #!optional
              (lift1 (macro-absent-obj))
              (lift2 (macro-absent-obj))
              (lift3 (macro-absent-obj))
              #!rest
              others)
  (macro-force-vars (receiver)
    (macro-check-procedure
     receiver
     1
     (call-with-current-continuation receiver lift1 lift2 lift3 . others)
     (cond ((##eq? lift1 (macro-absent-obj))
            (##call-with-current-continuation receiver))
           ((##eq? lift2 (macro-absent-obj))
            (##call-with-current-continuation receiver lift1))
           ((##eq? lift3 (macro-absent-obj))
            (##call-with-current-continuation receiver lift1 lift2))
           ((##null? others)
            (##call-with-current-continuation receiver lift1 lift2 lift3))
           (else
            (##apply
             ##call-with-current-continuation
             (##cons receiver
                     (##cons lift1
                             (##cons lift2
                                     (##cons lift3
                                             others))))))))))

(define call/cc
  call-with-current-continuation)

;;;----------------------------------------------------------------------------

;; Put primordial thread in ready queue

(let ((pt (##current-thread)))
  (##structure-type-set! pt (macro-type-thread)) ;; assign type descriptor
  (macro-thread-mutex-set! pt (##make-mutex (##void))) ;; mutex
  (macro-thread-condvar-set! pt (##make-condvar (##void))) ;; condvar
  (macro-thread-name-set! pt 'primordial) ;; name
  (macro-make-runnable! pt))

;;;============================================================================
