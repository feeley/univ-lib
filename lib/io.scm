;;;============================================================================

;;; File: "io.scm"

;;; Copyright (c) 2014 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;; Thread related data types

(define-type port
  id: B64DCF5B-7224-471A-B14A-1378EEA5162C
  extender: define-type-of-port
  macros:
  prefix: macro-
)

(define-type-of-port string-port
  id: 97F17491-8BED-488B-B37C-83A5BC02303A
  macros:
  prefix: macro-

  fifo
  pos-buf
  pos-in-buf
  pos-last-eol
  line-count
  eof?
  read-buf
  mutex
  condvar
)

(define-type-of-port vector-port
  id: 97F17491-8BED-488B-B37C-83A5BC02303A
  macros:
  prefix: macro-

  fifo
  mutex
  condvar
)

;;;----------------------------------------------------------------------------

;;; Representation of fifos.

(##define-macro (macro-make-fifo)
  `(let ((fifo (##cons '() '())))
     (macro-fifo-tail-set! fifo fifo)
     fifo))

(##define-macro (macro-fifo-next fifo)        `(##cdr ,fifo))
(##define-macro (macro-fifo-next-set! fifo x) `(##set-cdr! ,fifo ,x))
(##define-macro (macro-fifo-tail fifo)        `(##car ,fifo))
(##define-macro (macro-fifo-tail-set! fifo x) `(##set-car! ,fifo ,x))
(##define-macro (macro-fifo-elem fifo)        `(##car ,fifo))
(##define-macro (macro-fifo-elem-set! fifo x) `(##set-car! ,fifo ,x))

(##define-macro (macro-fifo->list fifo)
  `(macro-fifo-next ,fifo))

(##define-macro (macro-fifo-remove-all! fifo)
  `(let ((fifo ,fifo))

     (##declare (not interrupts-enabled))

     (let ((head (macro-fifo-next fifo)))
       (macro-fifo-tail-set! fifo fifo)
       (macro-fifo-next-set! fifo '())
       head)))

(##define-macro (macro-fifo-remove-head! fifo)
  `(let ((fifo ,fifo))

     (##declare (not interrupts-enabled))

     (let ((head (macro-fifo-next fifo)))
       (if (##pair? head)
         (let ((next (macro-fifo-next head)))
           (if (##null? next)
             (macro-fifo-tail-set! fifo fifo))
           (macro-fifo-next-set! fifo next)
           (macro-fifo-next-set! head '())))
       head)))

(##define-macro (macro-fifo-insert-at-tail! fifo elem)
  `(let ((fifo ,fifo) (elem ,elem))
     (let ((x (##cons elem '())))

       (##declare (not interrupts-enabled))

       (let ((tail (macro-fifo-tail fifo)))
         (macro-fifo-next-set! tail x)
         (macro-fifo-tail-set! fifo x)
         (##void)))))

(##define-macro (macro-fifo-insert-at-head! fifo elem)
  `(let ((fifo ,fifo) (elem ,elem))
     (let ((x (##cons elem '())))

       (##declare (not interrupts-enabled))

       (let ((head (macro-fifo-next fifo)))
         (if (##null? head)
           (macro-fifo-tail-set! fifo x))
         (macro-fifo-next-set! fifo x)
         (macro-fifo-next-set! x head)
         (##void)))))

(##define-macro (macro-fifo-advance-to-tail! fifo)
  `(let ((fifo ,fifo))
     ;; It is assumed that the fifo contains at least one element
     ;; (i.e. the fifo's tail does not change).
     (let ((new-head (macro-fifo-tail fifo)))
       (macro-fifo-next-set! fifo new-head)
       (macro-fifo-elem new-head))))

(##define-macro (macro-fifo-advance! fifo)
  `(let ((fifo ,fifo))
     ;; It is assumed that the fifo contains at least two elements
     ;; (i.e. the fifo's tail does not change).
     (let* ((head (macro-fifo-next fifo))
            (new-head (macro-fifo-next head)))
       (macro-fifo-next-set! fifo new-head)
       (macro-fifo-elem new-head))))

;;;----------------------------------------------------------------------------

;; String ports.

(define (##open-string)
  (macro-make-string-port
   (macro-make-fifo)       ;; fifo
   0
   0
   0
   0
   #f                      ;; eof?
   #f                      ;; read-buf
   (##make-mutex #f)       ;; mutex
   (##make-condvar #f)     ;; condvar
   ))

(define (##string-port-get-char! port)

  (##declare (not interrupts-enabled))

  ;; it is assumed that the port mutex is locked by the current thread

  (let loop ()
    (let* ((fifo (macro-string-port-fifo port))
           (head (macro-fifo-next fifo)))
      (if (##null? head)
          (if (macro-string-port-eof? port)
              (begin
                (macro-string-port-eof?-set! port #f)
                #!eof)
              (let ((mutex (macro-string-port-mutex port)))
                (##mutex-unlock! mutex (macro-string-port-condvar port))
                (##mutex-lock! mutex)
                (loop)))
          (let* ((buf (macro-fifo-elem head))
                 (pos (macro-string-port-pos-in-buf port)))
            (if (##fx< pos (##string-length buf))
                (##string-ref buf pos)
                (begin
                  (macro-string-port-pos-buf-set!
                   port
                   (##fx+ (macro-string-port-pos-buf port)
                          (macro-string-port-pos-in-buf port)))
                  (macro-string-port-pos-in-buf-set! port 0)
                  (macro-fifo-remove-head! fifo)
                  (loop))))))))

(define (##peek-read-char port read?)

  (##declare (not interrupts-enabled))

  (let ((mutex (macro-string-port-mutex port)))
    (##mutex-lock! mutex)
    (let ((c
           (or (macro-string-port-read-buf port)
               (let ((c (##string-port-get-char! port)))
                 (macro-string-port-read-buf-set! port c)
                 c))))
      (if read?
          (begin
            (macro-string-port-read-buf-set! port #f)
            (macro-string-port-pos-in-buf-set!
             port
             (##fx+ (macro-string-port-pos-in-buf port) 1))
            (if (##eqv? c #\newline)
                (begin
                  (macro-string-port-line-count-set!
                   port
                   (##fx+ (macro-string-port-line-count port) 1))
                  (macro-string-port-pos-last-eol-set!
                   port
                   (##fx+ (macro-string-port-pos-buf port)
                          (macro-string-port-pos-in-buf port)))))))
      (##mutex-unlock! mutex)
      c)))

(define (##peek-char port) (##peek-read-char port #f))
(define (##read-char port) (##peek-read-char port #t))

(define (##write-string str port)

  (##declare (not interrupts-enabled))

  (let ((mutex (macro-string-port-mutex port)))
    (##mutex-lock! mutex)
    (macro-fifo-insert-at-tail! (macro-string-port-fifo port) str)
    (##condvar-signal! (macro-string-port-condvar port))
    (##mutex-unlock! mutex)
    (##void)))

(define (##write-char c port)

  (##declare (not interrupts-enabled))

  (##write-string (##string c) port))

(define (##newline port)

  (##declare (not interrupts-enabled))

  (##write-string "\n" port))

(define (##write obj port)

  (##declare (not interrupts-enabled))

  (##write-string (generic-write-to-string obj #f #f) port))

(define (##pretty-print obj port)

  (##declare (not interrupts-enabled))

  (##write-string (generic-write-to-string obj #f 80) port))

(define (##write-eof port)

  (##declare (not interrupts-enabled))

  (let ((mutex (macro-string-port-mutex port)))
    (##mutex-lock! mutex)
    (macro-string-port-eof?-set! port #t)
    (##condvar-signal! (macro-string-port-condvar port))
    (##mutex-unlock! mutex)
    (##void)))

(define (##get-output-string port)
  (##append-strings
   (macro-fifo->list (macro-string-port-fifo port))))

(define (##input-port-line port)
  (##declare (not interrupts-enabled))
  (##fx+ (macro-string-port-line-count port) 1))

(define (##input-port-column port)
  (##declare (not interrupts-enabled))
  (##fx+ (##fx- (##fx+ (macro-string-port-pos-buf port)
                       (macro-string-port-pos-in-buf port))
                (macro-string-port-pos-last-eol port))
         1))

;;;----------------------------------------------------------------------------

;; Vector ports.

(define (##open-vector)
  (macro-make-vector-port
   (macro-make-fifo)   ;; fifo
   (##make-mutex #f)   ;; mutex
   (##make-condvar #f) ;; condvar
   ))

(define (##vector-port-get-object! port)

  (##declare (not interrupts-enabled))

  ;; it is assumed that the port mutex is locked by the current thread

  (let loop ()
    (let* ((fifo (macro-vector-port-fifo port))
           (head (macro-fifo-next fifo)))
      (if (##null? head)
          (let ((mutex (macro-vector-port-mutex port)))
            (##mutex-unlock! mutex (macro-vector-port-condvar port))
            (##mutex-lock! mutex)
            (loop))
          (let ((obj (macro-fifo-elem head)))
            (macro-fifo-remove-head! fifo)
            obj)))))

(define (##read-object port)

  (##declare (not interrupts-enabled))

  (let ((mutex (macro-vector-port-mutex port)))
    (##mutex-lock! mutex)
    (let ((obj (##vector-port-get-object! port)))
      (##mutex-unlock! mutex)
      obj)))

(define (##write-object obj port)

  (##declare (not interrupts-enabled))

  (let ((mutex (macro-vector-port-mutex port)))
    (##mutex-lock! mutex)
    (macro-fifo-insert-at-tail! (macro-vector-port-fifo port) obj)
    (##condvar-signal! (macro-vector-port-condvar port))
    (##mutex-unlock! mutex)
    (##void)))

;;;----------------------------------------------------------------------------

(define (##eof-object? x)
  (##eq? x #!eof))

(define (input-port? x)
  (macro-port? x)) ;; good enough for now

(define (output-port? x)
  (macro-port? x)) ;; good enough for now

;;;============================================================================
