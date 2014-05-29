

;;; File: "io.scm"

;;; Copyright (c) 2014 by Marc Feeley, All Rights Reserved.

;;;============================================================================


(macro-case-target
 ((js)
  (##inline-host-declaration 
"
var Gambit_js_fs = require('fs');
var Gambit_js_buf = require('buffer');

// For debugging purposes.
function Gambit_debug_registers_content() {
  console.log('\\n-------------------------- Trace Registers\\n')
  console.log('r0: ' +Gambit_r0 + '\\n');
  console.log('r1: ' +Gambit_r1 + '\\n');
  console.log('r2: ' +Gambit_r2 + '\\n');
  console.log('r3: ' +Gambit_r3 + '\\n');
  console.log('r4: ' +Gambit_r4 + '\\n');
}

function Gambit_io_open_file(filename, mode) {
  var fd = Gambit_js_fs.openSync(filename, mode);
  return fd;
}

function Gambit_io_close_file (fd) {
  Gambit_js_fs.closeSync(fd);
}
 
function Gambit_io_write_file (fd, s, pos) {
  var buf = new Gambit_js_buf.Buffer(s);
  Gambit_js_fs.writeSync(fd, buf, 0, buf.length, null);
}

function Gambit_io_read_file(fd) {
  var to_read = 50;
  var buf = new Gambit_js_buf.Buffer(to_read);
  var read = Gambit_js_fs.readSync(fd, buf, 0, buf.length, null);
  return buf.toString('utf8',0,read);
}
"
  ))

 ((python)
  (##inline-host-declaration
"
def Gambit_debug_registers_content():
    print '\\n-------------------------- Trace Registers\\n'
    print 'r0: ' + str(Gambit_r0) + '\\n'
    print 'r1: ' + str(Gambit_r1) + '\\n'
    print 'r2: ' + str(Gambit_r2) + '\\n'
    pritn 'r3: ' + str(Gambit_r3) + '\\n'
    print 'r4: ' + str(Gambit_r4) + '\\n'

def Gambit_io_open_file(fd):
    pass

def Gambit_io_close_file(fd):
    pass

def Gambit_io_write_file(fd):
    pass

def Gambit_io_read_file(fd):
    pass

def Gambit_py2scm(obj):
    if obj == None:
        return obj
    elif type(obj) is bool:
        return obj
    elif type(obj) is int or type(obj) is float:
        if obj>=-536870912 and obj <= 536870911:
            return obj
        else:
            return Gambit_Flonum(obj)
    elif type(obj) is function:
        return lambda () : return Gambit_scm2js(obj)
    elif type(obj) is instance :
        return obj
    elif type(obj) is bool:
        return obj



"
  ))

 ((ruby)
  (##inline-host-declaration
"
"
  ))

 ((php)
  (##inline-host-declaration
"
"
  )))

(macro-case-target
 ((js)
  (define js (##inline-host-expression "Gambit_js2scm(eval)")))
 ((python)
  (define python (##inline-host-expression "Gambit_py2scm(eval)")))
 ((ruby))
 ((php)))


(define (prn-ret str val)
  (println str)
  val)



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


;; Port hierarchy

(define (make-port-option) '())

(define-type port
  id: B64DCF5B-7224-471A-B14A-1378EEA5162C
  extender: define-type-of-port
  macros:
  prefix: macro-  

  name
  options
  mutex
  condvar
  close
)


(define-type-of-port object-port
  id: 179314a6-dc59-48d8-9c5f-43b824e0c38e
  extender: define-type-of-object-port
  macros:
  prefix: macro-
)


(define-type-of-object-port character-port
  id: 580398b3-3e0b-4329-b1da-970daa50310f
  extender: define-type-of-character-port
  macros:
  prefix: macro-

  wbuf
  rbuf
  wbuf-drain
  rbuf-fill
  rc-cache

  pos-buf
  pos-in-buf
  pos-last-eol
  line-count
  eof?
)


(define-type-of-character-port byte-port
  id: d2ac6d3b-d443-48ca-801d-a68aca83c563
  extender: define-type-of-byte-port
  macros:
  prefix: macro-
)


(define-type-of-byte-port device-port
  id: d340f2cd-4b0e-4a2b-b1e5-726aaa96e776
  extender: define-type-of-device-port
  macros:
  prefix: macro-
)


;; String port
;; -----------

(define-type-of-character-port string-port
  id: 97F17491-8BED-488B-B37C-83A5BC02303A
  macros:
  prefix: macro-

  write-eof
)


;; File-port
;; ---------

(define-type-of-device-port file-port
  id: 86433a9b-1698-499f-991d-cd18234fc268
  macros:
  prefix: macro-

  filename
  handle
  write-eof  
)


;; Vector Port
;; -----------

(define-type-of-object-port vector-port
  id: 97F17491-8BED-488B-B37C-83A5BC02303B
  macros:
  prefix: macro-

  fifo 
)


;;;----------------------------------------------------------------------------


;;
;; General port functions
;; ========================
;;

;; Port functions.
;; ----------------


;; Object port functions.
;; -----------------------

(define (##write obj port)
  (##declare (not interrupts-enabled))
  (##write-string (generic-write-to-string obj #f #f) port))

(define (##newline port)
  (##declare (not interrupts-enabled))
  (##write-string "\n" port))

(define (##pretty-print obj port)
  (##declare (not interrupts-enabled))
  (##write-string (generic-write-to-string obj #f 80) port))

(define (##close port)
  (##declare (not interrupts-enabled))
  ((macro-port-close port) port))



;;
;; Character port functions.
;; --------------------------

(define (##write-char c port)
  (##declare (not interrupts-enabled))
  (##write-string (##string c) port))

(define (##read-char port)
  (##declare (not interrupts-enabled))
  (##peek-read-char port #t))

(define (##peek-char port)
  (##declare (not interrupts-enabled))
  (##peek-read-char port #f))

(define (##readline port)
  (let loop ((s ""))
    (let ((c (##peek-char port)))
      (if (eq? c #!eof)
          s
          (if (eqv? (##read-char port) #\newline)
              s
              (loop (string-append s (string c))))))))

(define (##peek-read-char port read?)

  (##declare (not interrupts-enabled))

  (let ((mutex (macro-port-mutex port)))
    (##mutex-lock! mutex)
    (let ((c
           (or (macro-character-port-rc-cache port)
               (let ((c (##character-port-get-char! port)))
                 (macro-character-port-rc-cache-set! port c)
                 c))))
      (if read?
          (begin
            (macro-character-port-rc-cache-set! port #f)
            (macro-character-port-pos-in-buf-set!
             port
             (##fx+ (macro-character-port-pos-in-buf port) 1))
            (if (##eqv? c #\newline)
                (begin
                  (macro-character-port-line-count-set!
                   port
                  (##fx+ (macro-character-port-line-count port) 1))
                  (macro-character-port-pos-last-eol-set!
                   port
                   (##fx+ (macro-character-port-pos-buf port)
                          (macro-character-port-pos-in-buf port)))))))
      (##mutex-unlock! mutex)
      c)))

(define (##character-port-get-char! port)

  (##declare (not interrupts-enabled))

  ;; it is assumed that the port mutex is locked by the current thread

  (let loop ()
    (let ((pos (macro-character-port-pos-in-buf port))
          (buf (macro-character-port-rbuf port)))

      ;; An empty buffer means it should be filled if we don't get #!eof.
      (if (##string=? "" buf)
          (if (macro-character-port-eof? port)
              (begin
               (macro-character-port-eof?-set! port #f)
               #!eof)
              (let ((fill (macro-character-port-rbuf-fill port)))
                 (if fill
                   (begin (fill port)
                          ;; Initiate char reading!
                          (loop))
                   (println "ERROR - cannot read char"))))

          ;; The character to read is in the read buffer.
          (if (##fx< pos (##string-length buf))

            ;; Read the character
            (##string-ref buf pos)

            ;; Update counters and prepare the buffer to be filled.
            (begin
             (macro-character-port-rbuf-set! port "")
             (macro-character-port-pos-buf-set!
              port
              (##fx+ (macro-character-port-pos-buf port)
                     (macro-character-port-pos-in-buf port)))
             (macro-character-port-pos-in-buf-set! port 0)
             ;; Initiate filling!
             (loop)))))))
                

(define (##write-string str port)
  (##declare (not interrupts-enabled))
  (let ((mutex (macro-port-mutex port))
        (drain (macro-character-port-wbuf-drain port))
        (fifo  (macro-character-port-wbuf port)))
    (##mutex-lock! mutex)

    (if drain (drain port))

    (macro-fifo-insert-at-tail! fifo str)
    (##condvar-signal! (macro-port-condvar port))
    (##mutex-unlock! mutex)
    (##void)))

(define (##write-eof port)
  (##declare (not interrupts-enabled))
  (let ((mutex (macro-port-mutex port)))
    (##mutex-lock! mutex)
    (macro-character-port-eof?-set! port #t)
    (##condvar-signal! (macro-port-condvar port))
    (##mutex-unlock! mutex)
    (##void)))

(define (##get-output-string port)
  (##append-strings
   (macro-fifo->list (macro-character-port-wbuf port))))

(define (##input-port-line port)
  (##declare (not interrupts-enabled))
  (##fx+ (macro-character-port-line-count port) 1))

(define (##input-port-column port)
  (##declare (not interrupts-enabled))
  (##fx+ (##fx- (##fx+ (macro-character-port-pos-buf port)
                       (macro-character-port-pos-in-buf port))
                (macro-character-port-pos-last-eol port))
         1))

;; Byte port functions.
;; ---------------------


;; Device port functions.
;; -----------------------



;;
;; Specific port functions
;; ===========================
;;

;; String port functions
;; ----------------------

;; Open String

;(define (##string-port-drain port) 0)

(define (##string-port-fill port)
  (let* ((fifo (macro-character-port-wbuf port))
         (head (macro-fifo-next fifo)))
    (if (null? head)
        (let ((mutex (macro-port-mutex port)))
          (##mutex-unlock! mutex (macro-port-condvar port))
          (##mutex-lock! mutex))
        (begin
         (macro-character-port-rbuf-set! port (macro-fifo-elem head))
         (macro-fifo-remove-head! fifo)))
    (##character-port-get-char! port)))

(define (##string-port-close port)
  (let ((fifo (macro-character-port-wbuf port)))
    (macro-fifo-remove-all! fifo)))

(define (##open-string)
  (macro-make-string-port
   "string"            ;; name
   0                   ;; options
   (##make-mutex)      ;; mutex
   (##make-condvar)    ;; condvar
   ##string-port-close ;; close

   (macro-make-fifo)   ;; wbuf
   ""                  ;; rbuf
   #f                  ;; wbuf-drain
   ##string-port-fill  ;; rbuf-fill
   #f                  ;; rc-cache
   
   0 
   0
   0
   0
   #f                  ;; eof?

   ##write-eof         ;; write-eof
   ))



;; Vector port functions
;; ----------------------

(define (##open-vector)
  (macro-make-vector-port
   "vector"          ;; name
   0                 ;; options
   (##make-mutex)    ;; mutex
   (##make-condvar)  ;; condvar
   0                 ;; close
   (macro-make-fifo) ;; vector-fifo
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



;; File port functions
;; --------------------

(define (##open-file filename)        (##open-file-base filename 'rw))
(define (##open-input-file filename)  (##open-file-base filename 'w))
(define (##open-output-file filename) (##open-file-base filename 'r))

(define (##file-port-fill port)
  (let ((str (##inline-host-expression
              "Gambit_js2scm(Gambit_io_read_file(Gambit_scm2js(@1@)))"
               (macro-file-port-handle port))))
     (macro-character-port-rbuf-set! port str)
     (if (eq? (string-length str) 0)
         (macro-character-port-eof?-set! port #t))))

(define (##file-port-drain port)
  (let* ((fifo (macro-character-port-wbuf port))
         (str (##append-strings (macro-fifo->list fifo)))
         (handle (macro-file-port-handle port)))
    (macro-fifo-remove-all! fifo)
    (##inline-host-statement
     "Gambit_io_write_file(Gambit_scm2js(@1@), Gambit_scm2js(@2@));"
     handle
     str)))

(define (##file-port-close port)
  (let ((handle (macro-file-port-handle port)))
    (##file-port-drain port)
    (##inline-host-statement
     "Gambit_io_close_file(Gambit_scm2js(@1@));"
     handle)))

(define (##open-file-base filename mode)
  (define handle
   (##inline-host-expression
    "Gambit_io_open_file(Gambit_scm2js(@1@), Gambit_scm2js(@2@))"
    filename
    (cond
     ((eq? mode 'r)  "r")
     ((eq? mode 'rw) "a+")
     ((eq? mode 'w)  "a"))))

  (macro-make-file-port
   "file"                                  ;; name
   0                                       ;; options
   (##make-mutex)                          ;; mutex
   (##make-condvar)                        ;; condvar
   ##file-port-close                       ;; close

   (if (eq? mode 'r) #f (macro-make-fifo)) ;; wbuf 
   (if (eq? mode 'w) #f "")                ;; rbuf
   (if (eq? mode 'r) #f ##file-port-drain) ;; wbuf-drain
   (if (eq? mode 'w) #f ##file-port-fill)  ;; rbuf-fill
   #f                                      ;; rc-cache

   0
   0
   0
   0
   #f                                      ;; eof?
   filename                                ;; filename
   handle                                  ;; handle
   ##write-eof))

;;;----------------------------------------------------------------------------

(define (##eof-object? x)
  (##eq? x #!eof))

(define (input-port? x)
  (macro-port? x)) ;; good enough for now

(define (output-port? x)
  (macro-port? x)) ;; good enough for now

;;;============================================================================
