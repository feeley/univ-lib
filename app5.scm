;;; File: app5.scm

;;;----------------------------------------------------------------------------

(declare (standard-bindings) (extended-bindings) (not safe))

(##define-macro (shared-tag-mask)    #x80)
(##define-macro (shared-tag)         #x80)

(##define-macro (other-tag-mask)     #xf0)
(##define-macro (symbol-tag)         #x00)
(##define-macro (string-tag)         #x10)
(##define-macro (vector-tag)         #x20)
(##define-macro (structure-tag)      #x30)
(##define-macro (subprocedure-tag)   #x40)
(##define-macro (exact-int-tag)      #x50)

(##define-macro (character-tag)      #x60)
(##define-macro (flonum-tag)         #x61)
(##define-macro (ratnum-tag)         #x62)
(##define-macro (cpxnum-tag)         #x63)
(##define-macro (pair-tag)           #x64)
(##define-macro (continuation-tag)   #x65)
(##define-macro (boxvalues-tag)      #x66)
(##define-macro (ui-symbol-tag)      #x67)
(##define-macro (keyword-tag)        #x68)
(##define-macro (ui-keyword-tag)     #x69)
(##define-macro (closure-tag)        #x6a)
(##define-macro (frame-tag)          #x6b)
(##define-macro (gchashtable-tag)    #x6c)
(##define-macro (meroon-tag)         #x6d)
(##define-macro (jazz-tag)           #x6f) ;; note: tag is not consecutive
(##define-macro (homvector-tag)      #x6e)

(##define-macro (false-tag)          #x70)
(##define-macro (true-tag)           #x71)
(##define-macro (nil-tag)            #x72)
(##define-macro (eof-tag)            #x73)
(##define-macro (void-tag)           #x74)
(##define-macro (absent-tag)         #x75)
(##define-macro (unbound-tag)        #x76)
(##define-macro (unbound2-tag)       #x77)
(##define-macro (optional-tag)       #x78)
(##define-macro (key-tag)            #x79)
(##define-macro (rest-tag)           #x7a)
(##define-macro (unused-tag)         #x7b)
(##define-macro (deleted-tag)        #x7c)
(##define-macro (promise-tag)        #x7d)
(##define-macro (unassigned1-tag)    #x7e)
(##define-macro (unassigned2-tag)    #x7f)

(##define-macro (s8vector-tag)       #x00)
(##define-macro (u8vector-tag)       #x01)
(##define-macro (s16vector-tag)      #x02)
(##define-macro (u16vector-tag)      #x03)
(##define-macro (s32vector-tag)      #x04)
(##define-macro (u32vector-tag)      #x05)
(##define-macro (f32vector-tag)      #x06)
(##define-macro (s64vector-tag)      #x07)
(##define-macro (u64vector-tag)      #x08)
(##define-macro (f64vector-tag)      #x09)

(##define-macro (structure? obj) `(##structure? ,obj))
(##define-macro (gc-hash-table? obj) `(##gc-hash-table? ,obj))
(##define-macro (fixnum? obj) `(##fixnum? ,obj))

(define-prim (##object->u8vector
              obj
              #!optional
              (transform (macro-absent-obj)))

(##define-macro (subtype-set! obj subtype)
  `(##subtype-set! ,obj ,subtype))

(##define-macro (subvector-move! src-vect src-start src-end dst-vect dst-start)
  `(##subvector-move! ,src-vect ,src-start ,src-end ,dst-vect ,dst-start))

(##define-macro (max-fixnum)
  `##max-fixnum)

(##define-macro (max-char)
  `##max-char)


(##define-macro (continuation? obj)
  `(##continuation? ,obj))

(##define-macro (continuation-frame cont)
  `(##continuation-frame ,cont))

(##define-macro (continuation-denv cont)
  `(##continuation-denv ,cont))

(##define-macro (frame? obj)
  `(##frame? ,obj))

(##define-macro (frame-fs frame)
  `(##frame-fs ,frame))

(##define-macro (frame-ret frame)
  `(##frame-ret ,frame))

(##define-macro (frame-ref frame i)
  `(##frame-ref ,frame ,i))

(##define-macro (frame-slot-live? frame i)
  `(##frame-slot-live? ,frame ,i))

(##define-macro (subprocedure-parent-name subproc)
  `(##subprocedure-parent-name ,subproc))

(##define-macro (subprocedure-id subproc)
  `(##subprocedure-id ,subproc))

(##define-macro (subprocedure-nb-closed subproc)
  `(##subprocedure-nb-closed ,subproc))

(##define-macro (closure? obj)
  `(##closure? ,obj))

(##define-macro (make-closure code nb-closed)
  `(##make-closure ,code ,nb-closed))

(##define-macro (closure-code closure)
  `(##closure-code ,closure))

(##define-macro (closure-ref closure i)
  `(##closure-ref ,closure ,i))

(##define-macro (closure-set! closure i x)
  `(##closure-set! ,closure ,i ,x))

(##define-macro (extract-bit-field size position n)
  `(##extract-bit-field ,size ,position ,n))

(##define-macro (bignum? obj)
  `(##bignum? ,obj))

(##define-macro (subtyped? obj)
  `(##subtyped? ,obj))

(##define-macro (flonum? obj)
  `(##flonum? ,obj))

(##define-macro (ratnum? obj)
  `(##ratnum? ,obj))

(##define-macro (cpxnum? obj)
  `(##cpxnum? ,obj))

(##define-macro (boxvalues? obj)
  `(##fx= (##subtype ,obj) (macro-subtype-boxvalues)))

(##define-macro (promise? obj)
  `(##promise? ,obj))


(##define-macro (make-string . args)
  `(##make-string ,@args))

(##define-macro (string? . args)
  `(##string? ,@args))

(##define-macro (string-length str)
  `(##string-length ,str))

(##define-macro (string-ref str i)
  `(##string-ref ,str ,i))

(##define-macro (string-set! str i x)
  `(##string-set! ,str ,i ,x))


(##define-macro (make-vector . args)
  `(##make-vector ,@args))

(##define-macro (vector? . args)
  `(##vector? ,@args))

(##define-macro (vector-length vect)
  `(##vector-length ,vect))

(##define-macro (vector-ref vect i)
  `(##vector-ref ,vect ,i))

(##define-macro (vector-set! vect i x)
  `(##vector-set! ,vect ,i ,x))


(##define-macro (make-s8vector . args)
  `(##make-s8vector ,@args))

(##define-macro (s8vector? . args)
  `(##s8vector? ,@args))

(##define-macro (s8vector-length s8vect)
  `(##s8vector-length ,s8vect))

(##define-macro (s8vector-ref s8vect i)
  `(##s8vector-ref ,s8vect ,i))

(##define-macro (s8vector-set! s8vect i x)
  `(##s8vector-set! ,s8vect ,i ,x))

(##define-macro (s8vector-shrink! s8vect len)
  `(##s8vector-shrink! ,s8vect ,len))

(##define-macro (make-u8vector . args)
  `(##make-u8vector ,@args))

(##define-macro (u8vector? . args)
  `(##u8vector? ,@args))

(##define-macro (u8vector-length u8vect)
  `(##u8vector-length ,u8vect))

(##define-macro (u8vector-ref u8vect i)
  `(##u8vector-ref ,u8vect ,i))

(##define-macro (u8vector-set! u8vect i x)
  `(##u8vector-set! ,u8vect ,i ,x))

(##define-macro (u8vector-shrink! u8vect len)
  `(##u8vector-shrink! ,u8vect ,len))

(##define-macro (fifo->u8vector fifo start end)
  `(##fifo->u8vector ,fifo ,start ,end))


(##define-macro (make-s16vector . args)
  `(##make-s16vector ,@args))

(##define-macro (s16vector? . args)
  `(##s16vector? ,@args))

(##define-macro (s16vector-length s16vect)
  `(##s16vector-length ,s16vect))

(##define-macro (s16vector-ref s16vect i)
  `(##s16vector-ref ,s16vect ,i))

(##define-macro (s16vector-set! s16vect i x)
  `(##s16vector-set! ,s16vect ,i ,x))

(##define-macro (s16vector-shrink! s16vect len)
  `(##s16vector-shrink! ,s16vect ,len))

(##define-macro (make-u16vector . args)
  `(##make-u16vector ,@args))

(##define-macro (u16vector? . args)
  `(##u16vector? ,@args))

(##define-macro (u16vector-length u16vect)
  `(##u16vector-length ,u16vect))

(##define-macro (u16vector-ref u16vect i)
  `(##u16vector-ref ,u16vect ,i))

(##define-macro (u16vector-set! u16vect i x)
  `(##u16vector-set! ,u16vect ,i ,x))

(##define-macro (u16vector-shrink! u16vect len)
  `(##u16vector-shrink! ,u16vect ,len))


(##define-macro (make-s32vector . args)
  `(##make-s32vector ,@args))

(##define-macro (s32vector? . args)
  `(##s32vector? ,@args))

(##define-macro (s32vector-length s32vect)
  `(##s32vector-length ,s32vect))

(##define-macro (s32vector-ref s32vect i)
  `(##s32vector-ref ,s32vect ,i))

(##define-macro (s32vector-set! s32vect i x)
  `(##s32vector-set! ,s32vect ,i ,x))

(##define-macro (s32vector-shrink! s32vect len)
  `(##s32vector-shrink! ,s32vect ,len))

(##define-macro (make-u32vector . args)
  `(##make-u32vector ,@args))

(##define-macro (u32vector? . args)
  `(##u32vector? ,@args))

(##define-macro (u32vector-length u32vect)
  `(##u32vector-length ,u32vect))

(##define-macro (u32vector-ref u32vect i)
  `(##u32vector-ref ,u32vect ,i))

(##define-macro (u32vector-set! u32vect i x)
  `(##u32vector-set! ,u32vect ,i ,x))

(##define-macro (u32vector-shrink! u32vect len)
  `(##u32vector-shrink! ,u32vect ,len))


(##define-macro (make-s64vector . args)
  `(##make-s64vector ,@args))

(##define-macro (s64vector? . args)
  `(##s64vector? ,@args))

(##define-macro (s64vector-length s64vect)
  `(##s64vector-length ,s64vect))

(##define-macro (s64vector-ref s64vect i)
  `(##s64vector-ref ,s64vect ,i))

(##define-macro (s64vector-set! s64vect i x)
  `(##s64vector-set! ,s64vect ,i ,x))

(##define-macro (s64vector-shrink! s64vect len)
  `(##s64vector-shrink! ,s64vect ,len))

(##define-macro (make-u64vector . args)
  `(##make-u64vector ,@args))

(##define-macro (u64vector? . args)
  `(##u64vector? ,@args))

(##define-macro (u64vector-length u64vect)
  `(##u64vector-length ,u64vect))

(##define-macro (u64vector-ref u64vect i)
  `(##u64vector-ref ,u64vect ,i))

(##define-macro (u64vector-set! u64vect i x)
  `(##u64vector-set! ,u64vect ,i ,x))

(##define-macro (u64vector-shrink! u64vect len)
  `(##u64vector-shrink! ,u64vect ,len))


(##define-macro (make-f32vector . args)
  `(##make-f32vector ,@args))

(##define-macro (f32vector? . args)
  `(##f32vector? ,@args))

(##define-macro (f32vector-length f32vect)
  `(##f32vector-length ,f32vect))

(##define-macro (f32vector-ref f32vect i)
  `(##f32vector-ref ,f32vect ,i))

(##define-macro (f32vector-set! f32vect i x)
  `(##f32vector-set! ,f32vect ,i ,x))

(##define-macro (f32vector-shrink! f32vect len)
  `(##f32vector-shrink! ,f32vect ,len))

(##define-macro (make-f64vector . args)
  `(##make-f64vector ,@args))

(##define-macro (f64vector? . args)
  `(##f64vector? ,@args))

(##define-macro (f64vector-length f64vect)
  `(##f64vector-length ,f64vect))

(##define-macro (f64vector-ref f64vect i)
  `(##f64vector-ref ,f64vect ,i))

(##define-macro (f64vector-set! f64vect i x)
  `(##f64vector-set! ,f64vect ,i ,x))

(##define-macro (f64vector-shrink! f64vect len)
  `(##f64vector-shrink! ,f64vect ,len))


(##define-macro (symbol? . args)
  `(##symbol? ,@args))

(##define-macro (symbol->string . args)
  `(##symbol->string ,@args))

(##define-macro (string->symbol . args)
  `(##string->symbol ,@args))

(##define-macro (keyword? . args)
  `(##keyword? ,@args))

(##define-macro (keyword->string . args)
  `(##keyword->string ,@args))

(##define-macro (string->keyword . args)
  `(##string->keyword ,@args))


(##define-macro (+ . args)
  `(##fx+ ,@args))

(##define-macro (- . args)
  `(##fx- ,@args))

(##define-macro (* . args)
  `(##fx* ,@args))

(##define-macro (< . args)
  `(##fx< ,@args))

(##define-macro (> . args)
  `(##fx> ,@args))

(##define-macro (= . args)
  `(##fx= ,@args))

(##define-macro (>= . args)
  `(##fx>= ,@args))

(##define-macro (<= . args)
  `(##fx<= ,@args))

(##define-macro (bitwise-and . args)
  `(##fxand ,@args))

(##define-macro (bitwise-ior . args)
  `(##fxior ,@args))

(##define-macro (arithmetic-shift-left . args)
  `(##fxarithmetic-shift-left ,@args))

(##define-macro (arithmetic-shift-right . args)
  `(##fxarithmetic-shift-right ,@args))

(##define-macro (generic.+ . args)
  `(##+ ,@args))

(##define-macro (generic.arithmetic-shift . args)
  `(##arithmetic-shift ,@args))

(##define-macro (generic.bit-set? . args)
  `(##bit-set? ,@args))

(##define-macro (generic.bitwise-ior . args)
  `(##bitwise-ior ,@args))

(##define-macro (generic.extract-bit-field . args)
  `(##extract-bit-field ,@args))

(##define-macro (generic.gcd . args)
  `(##gcd ,@args))

(##define-macro (generic.negative? . args)
  `(##negative? ,@args))

(##define-macro (integer-length . args)
  `(##integer-length ,@args))

(##define-macro (make-table . args)
  `(##make-table 0 #f #f #f ##eq?))

(##define-macro (table-ref . args)
  `(##table-ref ,@args))

(##define-macro (table-set! . args)
  `(##table-set! ,@args))

(##define-macro (uninterned-keyword? . args)
  `(##uninterned-keyword? ,@args))

(##define-macro (uninterned-symbol? . args)
  `(##uninterned-symbol? ,@args))


(##define-macro (char->integer . args)
  `(##char->integer ,@args))

(##define-macro (integer->char . args)
  `(##integer->char ,@args))


(##define-macro (vector . args)
  `(##vector ,@args))


(##define-macro (cons . args)
  `(##cons ,@args))

(##define-macro (pair? . args)
  `(##pair? ,@args))

(##define-macro (car . args)
  `(##car ,@args))

(##define-macro (cdr . args)
  `(##cdr ,@args))

(##define-macro (set-car! . args)
  `(##set-car! ,@args))

(##define-macro (set-cdr! . args)
  `(##set-cdr! ,@args))


(##define-macro (procedure? . args)
  `(##procedure? ,@args))

(##define-macro (char? . args)
  `(##char? ,@args))

(##define-macro (real? . args)
  `(##real? ,@args))

(##define-macro (not . args)
  `(##not ,@args))

(##define-macro (eq? . args)
  `(##eq? ,@args))

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

       ;; To obtain an atomic update of the fifo, we must force a
       ;; garbage-collection to occur right away if needed by the
       ;; ##cons, so that any finalization that might mutate this fifo
       ;; will be done before updating the fifo.

       (##check-heap-limit)

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


  (define (cannot-serialize obj)
    (error "can't serialize" obj))

  (define chunk-len 256) ;; must be a power of 2

  (define state
    (vector 0
            (macro-make-fifo)
            0
            (make-table test: ##eq?)
            (if (eq? transform (macro-absent-obj))
                (lambda (x) x)
                transform)))

  (define (write-u8 x)
    (let ((ptr (vector-ref state 0)))
      (vector-set! state 0 (+ ptr 1))
      (let ((fifo (vector-ref state 1))
            (i (bitwise-and ptr (- chunk-len 1))))
        (u8vector-set!
         (if (= i 0)
             (let ((chunk (make-u8vector chunk-len)))
               (macro-fifo-insert-at-tail! fifo chunk)
               chunk)
             (macro-fifo-elem (macro-fifo-tail fifo)))
         i
         x))))

  (define (get-output-u8vector)
    (let ((ptr (vector-ref state 0))
          (fifo (vector-ref state 1)))
      (if (and (< 0 ptr) (<= ptr chunk-len))
          (let ((u8vect (macro-fifo-elem (macro-fifo-tail fifo))))
            (u8vector-shrink! u8vect ptr)
            u8vect)
          (fifo->u8vector fifo 0 ptr))))

  (define (share obj)
    (let ((n (table-ref (vector-ref state 3) obj #f)))
      (if n
          (begin
            (serialize-shared! n)
            #t)
          #f)))

  (define (alloc! obj)
    (let ((n (vector-ref state 2)))
      (vector-set! state 2 (+ n 1))
      (table-set! (vector-ref state 3) obj n)))

  (define (serialize-shared! n)
    (let ((lo (bitwise-and n #x7f))
          (hi (arithmetic-shift-right n 7)))
      (write-u8 (bitwise-ior (shared-tag) lo))
      (serialize-nonneg-fixnum! hi)))

  (define (serialize-nonneg-fixnum! n)
    (let ((lo (bitwise-and n #x7f))
          (hi (arithmetic-shift-right n 7)))
      (if (= hi 0)
          (write-u8 lo)
          (begin
            (write-u8 (bitwise-ior #x80 lo))
            (serialize-nonneg-fixnum! hi)))))

  (define (serialize-flonum-32! n)
    (serialize-exact-int-of-length!
     (##flonum->ieee754-32 n)
     4))

  (define (serialize-flonum-64! n)
    (serialize-exact-int-of-length!
     (##flonum->ieee754-64 n)
     8))

  (define (serialize-exact-int-of-length! n len)
    (if (fixnum? n)
        (let loop ((n n) (len len))
          (if (> len 0)
              (begin
                (write-u8 (bitwise-and n #xff))
                (loop (arithmetic-shift-right n 8) (- len 1)))))
        (let* ((len/2 (arithmetic-shift-right len 1))
               (len/2*8 (* len/2 8)))
          (serialize-exact-int-of-length!
           (generic.extract-bit-field len/2*8 0 n)
           len/2)
          (serialize-exact-int-of-length!
           (generic.arithmetic-shift n (- len/2*8))
           (- len len/2)))))

  (define (exact-int-length n signed?)
    (arithmetic-shift-right
     (+ (integer-length n) (if signed? 8 7))
     3))

  (define (serialize-exact-int! n)
    (or (share n)
        (let ((len (exact-int-length n #t)))
          (if (<= len 4)
              (write-u8 (bitwise-ior (exact-int-tag) (- #x0f len)))
              (begin
                (write-u8 (bitwise-ior (exact-int-tag) #x0f))
                (serialize-nonneg-fixnum! len)))
          (serialize-exact-int-of-length! n len)
          (alloc! n))))

  (define (serialize-vector-like! vect tag)
    (let ((len (vector-length vect)))
      (if (< len #x0f)
          (begin
            (write-u8 (bitwise-ior tag len))
            (serialize-subvector! vect 0 len))
          (serialize-vector-like-long! vect (bitwise-ior tag #x0f)))))

  (define (serialize-vector-like-long! vect tag)
    (let ((len (vector-length vect)))
      (write-u8 tag)
      (serialize-nonneg-fixnum! len)
      (serialize-subvector! vect 0 len)))

  (define (serialize-subvector! vect start end)
    (let loop ((i start))
      (if (< i end)
          (begin
            (serialize! (vector-ref vect i))
            (loop (+ i 1))))))

  (define (serialize-string-like! str tag mask)
    (let ((len (string-length str)))
      (if (< len mask)
          (begin
            (write-u8 (bitwise-ior tag len))
            (serialize-string! str))
          (begin
            (write-u8 (bitwise-ior tag mask))
            (serialize-nonneg-fixnum! len)
            (serialize-string! str)))))

  (define (serialize-string! str)
    (serialize-elements!
     0
     (string-length str)
     (lambda (i)
       (serialize-nonneg-fixnum! (char->integer (string-ref str i))))))

  (define (serialize-elements! start end serialize-element!)
    (let loop ((i start))
      (if (< i end)
          (begin
            (serialize-element! i)
            (loop (+ i 1))))))

  (define (serialize-homintvector! vect vect-tag vect-length vect-ref elem-len)
    (or (share vect)
        (let ((len (vect-length vect)))
          (write-u8 (homvector-tag))
          (serialize-nonneg-fixnum!
           (bitwise-ior vect-tag (arithmetic-shift-left len 4)))
          (serialize-elements!
           0
           len
           (lambda (i)
             (serialize-exact-int-of-length!
              (vect-ref vect i)
              elem-len)))
          (alloc! vect))))

  (define (serialize-homfloatvector! vect vect-tag vect-length vect-ref f32?)
    (or (share vect)
        (let ((len (vect-length vect)))
          (write-u8 (homvector-tag))
          (serialize-nonneg-fixnum!
           (bitwise-ior vect-tag (arithmetic-shift-left len 4)))
          (serialize-elements!
           0
           len
           (lambda (i)
             (let ((n (vect-ref vect i)))
               (if f32?
                   (serialize-flonum-32! n)
                   (serialize-flonum-64! n)))))
          (alloc! vect))))

  (define (serialize-subprocedure! subproc tag mask)
    (or (share subproc)
        (let ((parent-name (subprocedure-parent-name subproc)))
          (if (not parent-name)
              (cannot-serialize subproc)
              (let ((subproc-id (subprocedure-id subproc)))
                (if (< subproc-id mask)
                    (write-u8 (bitwise-ior tag subproc-id))
                    (begin
                      (write-u8 (bitwise-ior tag mask))
                      (serialize-nonneg-fixnum! subproc-id)))
                (serialize! (##system-version))
                (or (share parent-name)
                    (let ((str (symbol->string parent-name)))
                      (serialize-string-like! str 0 #x7f)
                      (alloc! parent-name)))
                (alloc! subproc))))))

  (define (serialize! obj)
    (let* ((transform (vector-ref state 4))
           (obj (transform obj)))
      (cond ((pair? obj)
             (or (share obj)
                 (begin
                   (alloc! obj)
                   (write-u8 (pair-tag))
                   (serialize! (car obj))
                   (serialize! (cdr obj)))))

;;            ((subtyped? obj)

;;             (cond 
                   ((symbol? obj)
                    (or (share obj)
                        (begin
                          (if (uninterned-symbol? obj)
                              (begin
                                (write-u8 (ui-symbol-tag))
                                (serialize-string-like!
                                 (symbol->string obj)
                                 0
                                 #xff)
                                (serialize-exact-int-of-length!
                                 (##symbol-hash obj)
                                 4))
                              (serialize-string-like!
                               (symbol->string obj)
                               (symbol-tag)
                               #x0f))
                          (write-u8 (if (##global-var? obj) 1 0))
                          (alloc! obj))))

                   ((keyword? obj)
                    (or (share obj)
                        (begin
                          (if (uninterned-keyword? obj)
                              (begin
                                (write-u8 (ui-keyword-tag))
                                (serialize-string-like!
                                 (keyword->string obj)
                                 0
                                 #xff)
                                (serialize-exact-int-of-length!
                                 (##keyword-hash obj)
                                 4))
                              (serialize-string-like!
                               (keyword->string obj)
                               (keyword-tag)
                               0))
                          (alloc! obj))))

                   ((string? obj)
                    (or (share obj)
                        (begin
                          (serialize-string-like!
                           obj
                           (string-tag)
                           #x0f)
                          (alloc! obj))))

                   ((vector? obj)
                    (or (share obj)
                        (begin
                          (alloc! obj)
                          (serialize-vector-like! obj (vector-tag)))))

                   ((structure? obj)
                    (if (or (macro-thread? obj)
                            (macro-tgroup? obj)
                            (macro-mutex? obj)
                            (macro-condvar? obj))
                      (cannot-serialize obj)
                      (or (share obj)
                          (begin
                            (alloc! obj)
                            (serialize-vector-like! obj (structure-tag))))))

                   ((procedure? obj)
                    (if (closure? obj)

                        (or (share obj)
                            (begin
                              (write-u8 (closure-tag))
                              (let* ((subproc
                                      (closure-code obj))
                                     (nb-closed
                                      (subprocedure-nb-closed subproc)))
                                (serialize-subprocedure! subproc 0 #x7f)
                                (alloc! obj)
                                (let loop ((i 1))
                                  (if (<= i nb-closed)
                                      (begin
                                        (serialize! (closure-ref obj i))
                                        (loop (+ i 1))))))))

                        (serialize-subprocedure! obj (subprocedure-tag) #x0f)))

                   ((flonum? obj)
                    (or (share obj)
                        (begin
                          (write-u8 (flonum-tag))
                          (serialize-flonum-64! obj)
                          (alloc! obj))))

#;
                   ((bignum? obj)
                    (serialize-exact-int! obj))
#;
                   ((ratnum? obj)
                    (or (share obj)
                        (begin
                          (write-u8 (ratnum-tag))
                          (serialize! (macro-ratnum-numerator obj))
                          (serialize! (macro-ratnum-denominator obj))
                          (alloc! obj))))
#;
                   ((cpxnum? obj)
                    (or (share obj)
                        (begin
                          (write-u8 (cpxnum-tag))
                          (serialize! (macro-cpxnum-real obj))
                          (serialize! (macro-cpxnum-imag obj))
                          (alloc! obj))))

                   ((continuation? obj)
                    (let ()

                      (define (serialize-cont-frame! cont)
                        (write-u8 (frame-tag))
                        (let ((subproc (##continuation-ret cont))
                              (fs (##continuation-fs cont)))
                          (serialize-subprocedure! subproc 0 #x7f)
                          (alloc! (##cons 11 22))
                          (let loop ((i fs))
                            (if (##fx> i 0)
                                (begin
                                  (serialize-cont-frame-ref! cont i)
                                  (loop (##fx- i 1)))))))

                      (define (serialize-cont-frame-ref! cont i)
                        (let* ((fs (##continuation-fs cont))
                               (j (##fx+ (##fx- fs i) 1)))
                          (if (##continuation-slot-live? cont j)
                              (if (##fx= j (##fx+ (##continuation-link cont) 1))
                                  (let ((next (##continuation-next cont)))
                                    (if next
                                        (serialize-cont-frame! next)
                                        (serialize! 0)))
                                  (serialize! (##continuation-ref cont j))))))

                      (or (share obj)
                          (begin
                            (alloc! obj)
                            (write-u8 (continuation-tag))
                            (serialize-cont-frame! obj)
                            (serialize! (continuation-denv obj))))))

                   ((frame? obj)
                    (or (share obj)
                        (begin
                          (write-u8 (frame-tag))
                          (let* ((subproc (frame-ret obj))
                                 (fs (frame-fs obj)))
                            (serialize-subprocedure! subproc 0 #x7f)
                            (alloc! obj)
                            (let loop ((i 1))
                              (if (<= i fs)
                                  (begin
                                    (if (frame-slot-live? obj i)
                                        (serialize! (frame-ref obj i)))
                                    (loop (+ i 1)))))))))
#;
                   ((boxvalues? obj)
                    (or (share obj)
                        (begin
                          (alloc! obj)
                          (serialize-vector-like-long! obj (boxvalues-tag)))))

#;
                   ((gc-hash-table? obj)
                    (or (share obj)
                        (begin
                          (alloc! obj)
                          (write-u8 (gchashtable-tag))
                          (let ()
                            (##declare (not interrupts-enabled))
                            (let ((len
                                   (vector-length obj))
                                  (flags
                                   (macro-gc-hash-table-flags obj))
                                  (count
                                   (macro-gc-hash-table-count obj))
                                  (min-count
                                   (macro-gc-hash-table-min-count obj))
                                  (free
                                   (macro-gc-hash-table-free obj)))
                              (serialize-nonneg-fixnum! len)
                              (serialize-nonneg-fixnum! flags)
                              (serialize-nonneg-fixnum! count)
                              (serialize-nonneg-fixnum! min-count)
                              (serialize-nonneg-fixnum! free))
                            (let loop ((i (macro-gc-hash-table-key0)))
                              (if (< i (vector-length obj))
                                  (let ((key (vector-ref obj i)))
                                    (if (and (not (eq? key (macro-unused-obj)))
                                             (not (eq? key (macro-deleted-obj))))
                                        (let ((val (vector-ref obj (+ i 1))))
                                          (serialize! key)
                                          (serialize! val)))
                                    (let ()
                                      (##declare (interrupts-enabled))
                                      (loop (+ i 2))))
                                  (serialize! (macro-unused-obj))))))))

#;
                   ((s8vector? obj)
                    (serialize-homintvector!
                     obj
                     (s8vector-tag)
                     (lambda (v) (s8vector-length v))
                     (lambda (v i) (s8vector-ref v i))
                     1))

                   ((u8vector? obj)
                    (serialize-homintvector!
                     obj
                     (u8vector-tag)
                     (lambda (v) (u8vector-length v))
                     (lambda (v i) (u8vector-ref v i))
                     1))
#;
                   ((s16vector? obj)
                    (serialize-homintvector!
                     obj
                     (s16vector-tag)
                     (lambda (v) (s16vector-length v))
                     (lambda (v i) (s16vector-ref v i))
                     2))
#;
                   ((u16vector? obj)
                    (serialize-homintvector!
                     obj
                     (u16vector-tag)
                     (lambda (v) (u16vector-length v))
                     (lambda (v i) (u16vector-ref v i))
                     2))
#;
                   ((s32vector? obj)
                    (serialize-homintvector!
                     obj
                     (s32vector-tag)
                     (lambda (v) (s32vector-length v))
                     (lambda (v i) (s32vector-ref v i))
                     4))
#;
                   ((u32vector? obj)
                    (serialize-homintvector!
                     obj
                     (u32vector-tag)
                     (lambda (v) (u32vector-length v))
                     (lambda (v i) (u32vector-ref v i))
                     4))
#;
                   ((s64vector? obj)
                    (serialize-homintvector!
                     obj
                     (s64vector-tag)
                     (lambda (v) (s64vector-length v))
                     (lambda (v i) (s64vector-ref v i))
                     8))
#;
                   ((u64vector? obj)
                    (serialize-homintvector!
                     obj
                     (u64vector-tag)
                     (lambda (v) (u64vector-length v))
                     (lambda (v i) (u64vector-ref v i))
                     8))
#;
                   ((f32vector? obj)
                    (serialize-homfloatvector!
                     obj
                     (f32vector-tag)
                     (lambda (v) (f32vector-length v))
                     (lambda (v i) (f32vector-ref v i))
                     #t))
#;
                   ((f64vector? obj)
                    (serialize-homfloatvector!
                     obj
                     (f64vector-tag)
                     (lambda (v) (f64vector-length v))
                     (lambda (v i) (f64vector-ref v i))
                     #f))
#;
                   ((promise? obj)
                    (or (share obj)
                        (begin
                          (alloc! obj)
                          (serialize-vector-like-long! obj (promise-tag)))))

;;                   (else
;;                    (cannot-serialize obj))))

            ((fixnum? obj)
             (cond ((and (>= obj #x00)
                         (< obj #x0b))
                    (write-u8 (bitwise-ior (exact-int-tag) obj)))
                   ((and (>= obj #x-80)
                         (< obj #x80))
                    (write-u8 (bitwise-ior (exact-int-tag) #x0e))
                    (write-u8 (bitwise-and obj #xff)))
                   (else
                    (serialize-exact-int! obj))))

            ((char? obj)
             (let ((n (char->integer obj)))
               (write-u8 (character-tag))
               (serialize-nonneg-fixnum! n)))

            ((eq? obj #f)                  (write-u8 (false-tag)))
            ((eq? obj #t)                  (write-u8 (true-tag)))
            ((eq? obj '())                 (write-u8 (nil-tag)))
            ((eq? obj #!eof)               (write-u8 (eof-tag)))
            ((eq? obj #!void)              (write-u8 (void-tag)))
            ((eq? obj (macro-absent-obj))  (write-u8 (absent-tag)))
            ((eq? obj #!unbound)           (write-u8 (unbound-tag)))
            ((eq? obj #!unbound2)          (write-u8 (unbound2-tag)))
            ((eq? obj #!optional)          (write-u8 (optional-tag)))
            ((eq? obj #!key)               (write-u8 (key-tag)))
            ((eq? obj #!rest)              (write-u8 (rest-tag)))
#;
            ((eq? obj (macro-unused-obj))  (write-u8 (unused-tag)))
#;
            ((eq? obj (macro-deleted-obj)) (write-u8 (deleted-tag)))

            (else
             (cannot-serialize obj)))))

  (serialize! obj)

  (get-output-u8vector))

(define-prim (object->u8vector
              obj
              #!optional
              (transform (macro-absent-obj)))
  (macro-force-vars (obj transform)
    (if (eq? transform (macro-absent-obj))
        (##object->u8vector obj)
        (macro-check-procedure transform 2 (object->u8vector obj transform)
          (##object->u8vector obj transform)))))

(define-prim (##u8vector->object
              u8vect
              #!optional
              (transform (macro-absent-obj)))

(##define-macro (subtype-set! obj subtype)
  `(##subtype-set! ,obj ,subtype))

(##define-macro (subvector-move! src-vect src-start src-end dst-vect dst-start)
  `(##subvector-move! ,src-vect ,src-start ,src-end ,dst-vect ,dst-start))

(##define-macro (max-fixnum)
  `##max-fixnum)

(##define-macro (max-char)
  `##max-char)


(##define-macro (continuation? obj)
  `(##continuation? ,obj))

(##define-macro (continuation-frame cont)
  `(##continuation-frame ,cont))

(##define-macro (continuation-denv cont)
  `(##continuation-denv ,cont))

(##define-macro (frame? obj)
  `(##frame? ,obj))

(##define-macro (frame-fs frame)
  `(##frame-fs ,frame))

(##define-macro (frame-ret frame)
  `(##frame-ret ,frame))

(##define-macro (frame-ref frame i)
  `(##frame-ref ,frame ,i))

(##define-macro (frame-slot-live? frame i)
  `(##frame-slot-live? ,frame ,i))

(##define-macro (subprocedure-parent-name subproc)
  `(##subprocedure-parent-name ,subproc))

(##define-macro (subprocedure-id subproc)
  `(##subprocedure-id ,subproc))

(##define-macro (subprocedure-nb-closed subproc)
  `(##subprocedure-nb-closed ,subproc))

(##define-macro (closure? obj)
  `(##closure? ,obj))

(##define-macro (make-closure code nb-closed)
  `(##make-closure ,code ,nb-closed))

(##define-macro (closure-code closure)
  `(##closure-code ,closure))

(##define-macro (closure-ref closure i)
  `(##closure-ref ,closure ,i))

(##define-macro (closure-set! closure i x)
  `(##closure-set! ,closure ,i ,x))

(##define-macro (extract-bit-field size position n)
  `(##extract-bit-field ,size ,position ,n))

(##define-macro (bignum? obj)
  `(##bignum? ,obj))

(##define-macro (subtyped? obj)
  `(##subtyped? ,obj))

(##define-macro (flonum? obj)
  `(##flonum? ,obj))

(##define-macro (ratnum? obj)
  `(##ratnum? ,obj))

(##define-macro (cpxnum? obj)
  `(##cpxnum? ,obj))

(##define-macro (boxvalues? obj)
  `(##fx= (##subtype ,obj) (macro-subtype-boxvalues)))


(##define-macro (make-string . args)
  `(##make-string ,@args))

(##define-macro (string? . args)
  `(##string? ,@args))

(##define-macro (string-length str)
  `(##string-length ,str))

(##define-macro (string-ref str i)
  `(##string-ref ,str ,i))

(##define-macro (string-set! str i x)
  `(##string-set! ,str ,i ,x))


(##define-macro (make-vector . args)
  `(##make-vector ,@args))

(##define-macro (vector? . args)
  `(##vector? ,@args))

(##define-macro (vector-length vect)
  `(##vector-length ,vect))

(##define-macro (vector-ref vect i)
  `(##vector-ref ,vect ,i))

(##define-macro (vector-set! vect i x)
  `(##vector-set! ,vect ,i ,x))


(##define-macro (make-s8vector . args)
  `(##make-s8vector ,@args))

(##define-macro (s8vector? . args)
  `(##s8vector? ,@args))

(##define-macro (s8vector-length s8vect)
  `(##s8vector-length ,s8vect))

(##define-macro (s8vector-ref s8vect i)
  `(##s8vector-ref ,s8vect ,i))

(##define-macro (s8vector-set! s8vect i x)
  `(##s8vector-set! ,s8vect ,i ,x))

(##define-macro (s8vector-shrink! s8vect len)
  `(##s8vector-shrink! ,s8vect ,len))

(##define-macro (make-u8vector . args)
  `(##make-u8vector ,@args))

(##define-macro (u8vector? . args)
  `(##u8vector? ,@args))

(##define-macro (u8vector-length u8vect)
  `(##u8vector-length ,u8vect))

(##define-macro (u8vector-ref u8vect i)
  `(##u8vector-ref ,u8vect ,i))

(##define-macro (u8vector-set! u8vect i x)
  `(##u8vector-set! ,u8vect ,i ,x))

(##define-macro (u8vector-shrink! u8vect len)
  `(##u8vector-shrink! ,u8vect ,len))

(##define-macro (fifo->u8vector fifo start end)
  `(##fifo->u8vector ,fifo ,start ,end))


(##define-macro (make-s16vector . args)
  `(##make-s16vector ,@args))

(##define-macro (s16vector? . args)
  `(##s16vector? ,@args))

(##define-macro (s16vector-length s16vect)
  `(##s16vector-length ,s16vect))

(##define-macro (s16vector-ref s16vect i)
  `(##s16vector-ref ,s16vect ,i))

(##define-macro (s16vector-set! s16vect i x)
  `(##s16vector-set! ,s16vect ,i ,x))

(##define-macro (s16vector-shrink! s16vect len)
  `(##s16vector-shrink! ,s16vect ,len))

(##define-macro (make-u16vector . args)
  `(##make-u16vector ,@args))

(##define-macro (u16vector? . args)
  `(##u16vector? ,@args))

(##define-macro (u16vector-length u16vect)
  `(##u16vector-length ,u16vect))

(##define-macro (u16vector-ref u16vect i)
  `(##u16vector-ref ,u16vect ,i))

(##define-macro (u16vector-set! u16vect i x)
  `(##u16vector-set! ,u16vect ,i ,x))

(##define-macro (u16vector-shrink! u16vect len)
  `(##u16vector-shrink! ,u16vect ,len))


(##define-macro (make-s32vector . args)
  `(##make-s32vector ,@args))

(##define-macro (s32vector? . args)
  `(##s32vector? ,@args))

(##define-macro (s32vector-length s32vect)
  `(##s32vector-length ,s32vect))

(##define-macro (s32vector-ref s32vect i)
  `(##s32vector-ref ,s32vect ,i))

(##define-macro (s32vector-set! s32vect i x)
  `(##s32vector-set! ,s32vect ,i ,x))

(##define-macro (s32vector-shrink! s32vect len)
  `(##s32vector-shrink! ,s32vect ,len))

(##define-macro (make-u32vector . args)
  `(##make-u32vector ,@args))

(##define-macro (u32vector? . args)
  `(##u32vector? ,@args))

(##define-macro (u32vector-length u32vect)
  `(##u32vector-length ,u32vect))

(##define-macro (u32vector-ref u32vect i)
  `(##u32vector-ref ,u32vect ,i))

(##define-macro (u32vector-set! u32vect i x)
  `(##u32vector-set! ,u32vect ,i ,x))

(##define-macro (u32vector-shrink! u32vect len)
  `(##u32vector-shrink! ,u32vect ,len))


(##define-macro (make-s64vector . args)
  `(##make-s64vector ,@args))

(##define-macro (s64vector? . args)
  `(##s64vector? ,@args))

(##define-macro (s64vector-length s64vect)
  `(##s64vector-length ,s64vect))

(##define-macro (s64vector-ref s64vect i)
  `(##s64vector-ref ,s64vect ,i))

(##define-macro (s64vector-set! s64vect i x)
  `(##s64vector-set! ,s64vect ,i ,x))

(##define-macro (s64vector-shrink! s64vect len)
  `(##s64vector-shrink! ,s64vect ,len))

(##define-macro (make-u64vector . args)
  `(##make-u64vector ,@args))

(##define-macro (u64vector? . args)
  `(##u64vector? ,@args))

(##define-macro (u64vector-length u64vect)
  `(##u64vector-length ,u64vect))

(##define-macro (u64vector-ref u64vect i)
  `(##u64vector-ref ,u64vect ,i))

(##define-macro (u64vector-set! u64vect i x)
  `(##u64vector-set! ,u64vect ,i ,x))

(##define-macro (u64vector-shrink! u64vect len)
  `(##u64vector-shrink! ,u64vect ,len))


(##define-macro (make-f32vector . args)
  `(##make-f32vector ,@args))

(##define-macro (f32vector? . args)
  `(##f32vector? ,@args))

(##define-macro (f32vector-length f32vect)
  `(##f32vector-length ,f32vect))

(##define-macro (f32vector-ref f32vect i)
  `(##f32vector-ref ,f32vect ,i))

(##define-macro (f32vector-set! f32vect i x)
  `(##f32vector-set! ,f32vect ,i ,x))

(##define-macro (f32vector-shrink! f32vect len)
  `(##f32vector-shrink! ,f32vect ,len))

(##define-macro (make-f64vector . args)
  `(##make-f64vector ,@args))

(##define-macro (f64vector? . args)
  `(##f64vector? ,@args))

(##define-macro (f64vector-length f64vect)
  `(##f64vector-length ,f64vect))

(##define-macro (f64vector-ref f64vect i)
  `(##f64vector-ref ,f64vect ,i))

(##define-macro (f64vector-set! f64vect i x)
  `(##f64vector-set! ,f64vect ,i ,x))

(##define-macro (f64vector-shrink! f64vect len)
  `(##f64vector-shrink! ,f64vect ,len))


(##define-macro (symbol? . args)
  `(##symbol? ,@args))

(##define-macro (symbol->string . args)
  `(##symbol->string ,@args))

(##define-macro (string->symbol . args)
  `(##string->symbol ,@args))

(##define-macro (keyword? . args)
  `(##keyword? ,@args))

(##define-macro (keyword->string . args)
  `(##keyword->string ,@args))

(##define-macro (string->keyword . args)
  `(##string->keyword ,@args))


(##define-macro (+ . args)
  `(##fx+ ,@args))

(##define-macro (- . args)
  `(##fx- ,@args))

(##define-macro (* . args)
  `(##fx* ,@args))

(##define-macro (< . args)
  `(##fx< ,@args))

(##define-macro (> . args)
  `(##fx> ,@args))

(##define-macro (= . args)
  `(##fx= ,@args))

(##define-macro (>= . args)
  `(##fx>= ,@args))

(##define-macro (<= . args)
  `(##fx<= ,@args))

(##define-macro (bitwise-and . args)
  `(##fxand ,@args))

(##define-macro (bitwise-ior . args)
  `(##fxior ,@args))

(##define-macro (arithmetic-shift-left . args)
  `(##fxarithmetic-shift-left ,@args))

(##define-macro (arithmetic-shift-right . args)
  `(##fxarithmetic-shift-right ,@args))

(##define-macro (generic.+ . args)
  `(##+ ,@args))

(##define-macro (generic.arithmetic-shift . args)
  `(##arithmetic-shift ,@args))

(##define-macro (generic.bit-set? . args)
  `(##bit-set? ,@args))

(##define-macro (generic.bitwise-ior . args)
  `(##bitwise-ior ,@args))

(##define-macro (generic.extract-bit-field . args)
  `(##extract-bit-field ,@args))

(##define-macro (generic.gcd . args)
  `(##gcd ,@args))

(##define-macro (generic.negative? . args)
  `(##negative? ,@args))

(##define-macro (integer-length . args)
  `(##integer-length ,@args))

(##define-macro (make-table . args)
  `(##make-table 0 #f #f #f ##eq?))

(##define-macro (table-ref . args)
  `(##table-ref ,@args))

(##define-macro (table-set! . args)
  `(##table-set! ,@args))

(##define-macro (uninterned-keyword? . args)
  `(##uninterned-keyword? ,@args))

(##define-macro (uninterned-symbol? . args)
  `(##uninterned-symbol? ,@args))


(##define-macro (char->integer . args)
  `(##char->integer ,@args))

(##define-macro (integer->char . args)
  `(##integer->char ,@args))


(##define-macro (vector . args)
  `(##vector ,@args))


(##define-macro (cons . args)
  `(##cons ,@args))

(##define-macro (pair? . args)
  `(##pair? ,@args))

(##define-macro (car . args)
  `(##car ,@args))

(##define-macro (cdr . args)
  `(##cdr ,@args))

(##define-macro (set-car! . args)
  `(##set-car! ,@args))

(##define-macro (set-cdr! . args)
  `(##set-cdr! ,@args))


(##define-macro (procedure? . args)
  `(##procedure? ,@args))

(##define-macro (char? . args)
  `(##char? ,@args))

(##define-macro (real? . args)
  `(##real? ,@args))

(##define-macro (not . args)
  `(##not ,@args))

(##define-macro (eq? . args)
  `(##eq? ,@args))

;; Representation of fifos.

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

       ;; To obtain an atomic update of the fifo, we must force a
       ;; garbage-collection to occur right away if needed by the
       ;; ##cons, so that any finalization that might mutate this fifo
       ;; will be done before updating the fifo.

       (##check-heap-limit)

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


  (define (err)
    (error "deserialization error"))

  (define state
    (vector 0
            u8vect
            0
            (make-vector 64)
            (if (eq? transform (macro-absent-obj))
                (lambda (x) x)
                transform)))

  (define (read-u8)
    (let ((ptr (vector-ref state 0))
          (u8vect (vector-ref state 1)))
      (if (< ptr (u8vector-length u8vect))
          (begin
            (vector-set! state 0 (+ ptr 1))
            ;;;(pretty-print (u8vector-ref u8vect ptr));;;;;;;;;;;;;;;;
            (u8vector-ref u8vect ptr))
          (error 111))))

  (define (eof?)
    (let ((ptr (vector-ref state 0))
          (u8vect (vector-ref state 1)))
      (= ptr (u8vector-length u8vect))))

  (define (alloc! obj)
    (let* ((n (vector-ref state 2))
           (vect (vector-ref state 3))
           (len (vector-length vect)))
      (vector-set! state 2 (+ n 1))
      (if (= n len)
          (let* ((new-len (+ (arithmetic-shift-right (* len 3) 1) 1))
                 (new-vect (make-vector new-len)))
            (vector-set! state 3 new-vect)
            (subvector-move! vect 0 n new-vect 0)
            (vector-set! new-vect n obj))
          (vector-set! vect n obj))
      n))

  (define (shared-ref i)
    (let* ((n (vector-ref state 2))
           (vect (vector-ref state 3)))
      (if (< i n)
          (vector-ref vect i)
          (error 222))))

  (define (deserialize-nonneg-fixnum! n shift)
    (let loop ((n n)
               (shift shift)
               (range (arithmetic-shift-right (max-fixnum) shift)))
      (if (= range 0)
          (error 333)
          (let ((x (read-u8)))
            (if (< x #x80)
                (if (< range x)
                    (error 444)
                    (bitwise-ior n (arithmetic-shift-left x shift)))
                (let ((b (bitwise-and x #x7f)))
                  (if (< range b)
                      (error 555)
                      (loop (bitwise-ior n (arithmetic-shift-left b shift))
                            (+ shift 7)
                            (arithmetic-shift-right range 7)))))))))

  (define (deserialize-flonum-32!)
    (let ((n (deserialize-nonneg-exact-int-of-length! 4)))
      (##ieee754-32->flonum n)))

  (define (deserialize-flonum-64!)
    (let ((n (deserialize-nonneg-exact-int-of-length! 8)))
      (##ieee754-64->flonum n)))

  (define (deserialize-nonneg-exact-int-of-length! len)
    (if (<= len 3) ;; result fits in a 32 bit fixnum?
        (let ((a (read-u8)))
          (if (= len 1)
              a
              (+ a
                 (arithmetic-shift-left
                  (let ((b (read-u8)))
                    (if (= len 2)
                        b
                        (+ b
                           (arithmetic-shift-left
                            (let ((c (read-u8)))
                              c)
                            8))))
                  8))))
        (let* ((len/2 (arithmetic-shift-right len 1))
               (a (deserialize-nonneg-exact-int-of-length! len/2))
               (b (deserialize-nonneg-exact-int-of-length! (- len len/2))))
          (generic.bitwise-ior a (generic.arithmetic-shift b (* 8 len/2))))))

  (define (deserialize-exact-int-of-length! len)
    (let ((n (deserialize-nonneg-exact-int-of-length! len)))
      (if (generic.bit-set? (- (* 8 len) 1) n)
          (generic.+ n (generic.arithmetic-shift -1 (* 8 len)))
          n)))

  (define (deserialize-string! x mask)
    (deserialize-string-of-length!
     (let ((lo (bitwise-and x mask)))
       (if (< lo mask)
           lo
           (deserialize-nonneg-fixnum! 0 0)))))

  (define (deserialize-string-of-length! len)
    (let ((obj (make-string len)))
      (let loop ((i 0))
        (if (< i len)
            (let ((n (deserialize-nonneg-fixnum! 0 0)))
              (if (<= n (max-char))
                  (begin
                    (string-set! obj i (integer->char n))
                    (loop (+ i 1)))
                  (error 666)))
            obj))))

  (define (deserialize-vector-like! subtype x)
    (let* ((len (bitwise-and x #x0f)))
      (if (< len #x0f)
          (deserialize-vector-like-fill! subtype len)
          (deserialize-vector-like-long! subtype))))

  (define (deserialize-vector-like-long! subtype)
    (let ((len (deserialize-nonneg-fixnum! 0 0)))
      (deserialize-vector-like-fill! subtype len)))

  (define (deserialize-vector-like-fill! subtype len)
    (let ((obj (make-vector len)))
      (alloc! obj)
      (let loop ((i 0))
        (if (< i len)
            (begin
              (vector-set! obj i (deserialize!))
              (loop (+ i 1)))
            (begin
              (subtype-set! obj subtype)
              obj)))))

  (define (deserialize-homintvector! make-vect vect-set! elem-len signed? len)
    (let ((obj (make-vect len)))
      (let loop ((i 0))
        (if (< i len)
            (begin
              (vect-set!
               obj
               i
               (if signed?
                   (deserialize-exact-int-of-length! elem-len)
                   (deserialize-nonneg-exact-int-of-length! elem-len)))
              (loop (+ i 1)))
            (begin
              (alloc! obj)
              obj)))))

  (define (deserialize-homfloatvector! make-vect vect-set! len f32?)
    (let ((obj (make-vect len)))
      (let loop ((i 0))
        (if (< i len)
            (begin
              (vect-set!
               obj
               i
               (if f32?
                   (deserialize-flonum-32!)
                   (deserialize-flonum-64!)))
              (loop (+ i 1)))
            (begin
              (alloc! obj)
              obj)))))

  (define (deserialize-subprocedure!)
    (let ((x (read-u8)))
      (if (>= x (shared-tag))
          (shared-ref
           (deserialize-nonneg-fixnum! (bitwise-and x #x7f) 7))
          (let ((subproc-id
                 (let ((id (bitwise-and x #x7f)))
                   (if (< id #x7f)
                       id
                       (deserialize-nonneg-fixnum! 0 0)))))
            (deserialize-subprocedure-with-id! subproc-id)))))

  (define (deserialize-subprocedure-with-id! subproc-id)
    (let ((v (deserialize!)))
      (if (not (eq? v (##system-version)))
          (error 777)
          (let* ((x
                  (read-u8))
                 (parent-name
                  (if (>= x (shared-tag))
                      (let ((name
                             (shared-ref
                              (deserialize-nonneg-fixnum!
                               (bitwise-and x #x7f)
                               7))))
                        (if (not (symbol? name))
                            (error 888)
                            name))
                      (let ((name
                             (string->symbol (deserialize-string! x #x7f))))
                        (alloc! name)
                        name)))
                 (parent
                  (##global-var-primitive-ref 
                   (##make-global-var parent-name))))
            (if (not (procedure? parent)) ;; should also check subproc-id
                (error 999)
                (let ((obj (##make-subprocedure parent subproc-id)))
                  (alloc! obj)
                  obj))))))

  (define (create-global-var-if-needed sym)
    (let ((x (read-u8)))
      (if (= x 1)
          (##make-global-var sym))))

  (define (deserialize-without-transform!)
    (let ((x (read-u8)))

      (cond ((>= x (shared-tag))
             (shared-ref
              (deserialize-nonneg-fixnum! (bitwise-and x #x7f) 7)))

            ((>= x (false-tag))
             (cond ((= x (false-tag))
                    #f)

                   ((= x (true-tag))
                    #t)

                   ((= x (nil-tag))
                    '())

                   ((= x (eof-tag))
                    #!eof)

                   ((= x (void-tag))
                    #!void)

                   ((= x (absent-tag))
                    (macro-absent-obj))

                   ((= x (unbound-tag))
                    #!unbound)

                   ((= x (unbound2-tag))
                    #!unbound2)

                   ((= x (optional-tag))
                    #!optional)

                   ((= x (key-tag))
                    #!key)

                   ((= x (rest-tag))
                    #!rest)

#;
                   ((= x (unused-tag))
                    (macro-unused-obj))

#;
                   ((= x (deleted-tag))
                    (macro-deleted-obj))

                   ((= x (promise-tag))
                    (deserialize-vector-like-long!
                     (macro-subtype-promise)))

                   (else
                    (error 1))))

            ((>= x (character-tag))
             (cond ((= x (character-tag))
                    (let ((n (deserialize-nonneg-fixnum! 0 0)))
                      (if (<= n (max-char))
                          (integer->char n)
                          (error 2))))

                   ((= x (flonum-tag))
                    (let ((obj (deserialize-flonum-64!)))
                      (alloc! obj)
                      obj))

                   ((= x (ratnum-tag))
                    (let* ((num (deserialize!))
                           (den (deserialize!)))
                      (if (or (and (fixnum? den)
                                   (<= den 1))
                              (and (bignum? den)
                                   (generic.negative? den))
                              (not (eq? 1 (generic.gcd num den))))
                          (error 3)
                          (let ((obj (macro-ratnum-make num den)))
                            (alloc! obj)
                            obj))))

                   ((= x (cpxnum-tag))
                    (let* ((real (deserialize!))
                           (imag (deserialize!)))
                      (if (or (not (real? real))
                              (not (real? imag)))
                          (error 4)
                          (let ((obj (macro-cpxnum-make real imag)))
                            (alloc! obj)
                            obj))))

                   ((= x (pair-tag))
                    (let ((obj (cons #f #f)))
                      (alloc! obj)
                      (let* ((a (deserialize!))
                             (d (deserialize!)))
                        (set-car! obj a)
                        (set-cdr! obj d)
                        obj)))

                   ((= x (continuation-tag))
                    (let ((obj (vector #f #f)))
                      (alloc! obj)
                      (let* ((frame (deserialize!))
                             (denv (deserialize!)))
                        (if (not (frame? frame)) ;; should also check denv
                            (error 5)
                            (begin
                              (vector-set! obj 0 frame)
                              (vector-set! obj 1 denv)
                              (subtype-set! obj (macro-subtype-continuation))
                              obj)))))

                   ((= x (boxvalues-tag))
                    (deserialize-vector-like-long!
                     (macro-subtype-boxvalues)))

                   ((= x (ui-symbol-tag))
                    (let* ((y (read-u8))
                           (name (deserialize-string! y #xff))
                           (hash (deserialize-exact-int-of-length! 4))
                           (obj (macro-make-uninterned-symbol name hash)))
                      (create-global-var-if-needed obj)
                      (alloc! obj)
                      obj))

                   ((= x (keyword-tag))
                    (let* ((name (deserialize-string! 0 0))
                           (obj (string->keyword name)))
                      (alloc! obj)
                      obj))

                   ((= x (ui-keyword-tag))
                    (let* ((y (read-u8))
                           (name (deserialize-string! y #xff))
                           (hash (deserialize-exact-int-of-length! 4))
                           (obj (macro-make-uninterned-keyword name hash)))
                      (alloc! obj)
                      obj))

                   ((= x (closure-tag))
                    (let ((subproc (deserialize-subprocedure!)))
                      (if #f;;;;;;;not subprocedure
                          (error 6)
                          (let ((nb-closed
                                 (subprocedure-nb-closed subproc)))
                            (if #f;;;;; nb-closed < 0
                                (error 7)
                                (let ((obj (make-closure subproc nb-closed)))
                                  (alloc! obj)
                                  (let loop ((i 1))
                                    (if (<= i nb-closed)
                                        (let ((x (deserialize!)))
                                          (closure-set! obj i x)
                                          (loop (+ i 1)))
                                        obj))))))))

                   ((= x (frame-tag))
                    (let ((subproc (deserialize-subprocedure!)))
                      (if (not (##return? subproc))
                          (error 8)
                          (let* ((fs (##return-fs subproc))
                                 (obj (make-vector (+ fs 1))))
                            (vector-set! obj 0 subproc)
                            (alloc! obj)
                            (let loop ((i 1))
                              (if (<= i fs)
                                  (begin
                                    (vector-set!
                                     obj
                                     (+ (- fs i) 1)
                                     (if (frame-slot-live? obj i)
                                         (deserialize!)
                                         0))
                                    (loop (+ i 1)))
                                  (begin
                                    (subtype-set! obj (macro-subtype-frame))
                                    obj)))))))

#;
                   ((= x (gchashtable-tag))
                    (let* ((len (deserialize-nonneg-fixnum! 0 0))
                           (flags (deserialize-nonneg-fixnum! 0 0))
                           (count (deserialize-nonneg-fixnum! 0 0))
                           (min-count (deserialize-nonneg-fixnum! 0 0))
                           (free (deserialize-nonneg-fixnum! 0 0)))
                      (if #f;;;;;;;;parameters OK?
                          (err)
                          (let ((obj (make-vector len (macro-unused-obj))))
                            (alloc! obj)
                            (macro-gc-hash-table-flags-set!
                             obj
                             (bitwise-ior ;; force rehash at next access!
                              flags
                              (+ (macro-gc-hash-table-flag-key-moved)
                                 (macro-gc-hash-table-flag-need-rehash))))
                            (macro-gc-hash-table-count-set! obj count)
                            (macro-gc-hash-table-min-count-set! obj min-count)
                            (macro-gc-hash-table-free-set! obj free)
                            (let loop ((i (macro-gc-hash-table-key0)))
                              (if (< i (vector-length obj))
                                  (let ((key (deserialize!)))
                                    (if (not (eq? key (macro-unused-obj)))
                                        (let ((val (deserialize!)))
                                          (vector-set! obj i key)
                                          (vector-set! obj (+ i 1) val)
                                          (loop (+ i 2)))
                                        (begin
                                          (subtype-set!
                                           obj
                                           (macro-subtype-weak))
                                          obj)))
                                  (err)))))))

                   ((= x (meroon-tag))
                    (deserialize-vector-like-long!
                     (macro-subtype-meroon)))

                   ((= x (jazz-tag))
                    (deserialize-vector-like-long!
                     (macro-subtype-jazz)))

                   ((= x (homvector-tag))
                    (let* ((len/type
                            (deserialize-nonneg-fixnum! 0 0))
                           (len
                            (arithmetic-shift-right len/type 4))
                           (type
                            (bitwise-and len/type #x0f)))
                      (cond ((= type (s8vector-tag))
                             (deserialize-homintvector!
                              (lambda (n) (make-s8vector n))
                              (lambda (v i n) (s8vector-set! v i n))
                              1
                              #t
                              len))
                            ((= type (u8vector-tag))
                             (deserialize-homintvector!
                              (lambda (n) (make-u8vector n))
                              (lambda (v i n) (u8vector-set! v i n))
                              1
                              #f
                              len))
                            ((= type (s16vector-tag))
                             (deserialize-homintvector!
                              (lambda (n) (make-s16vector n))
                              (lambda (v i n) (s16vector-set! v i n))
                              2
                              #t
                              len))
                            ((= type (u16vector-tag))
                             (deserialize-homintvector!
                              (lambda (n) (make-u16vector n))
                              (lambda (v i n) (u16vector-set! v i n))
                              2
                              #f
                              len))
                            ((= type (s32vector-tag))
                             (deserialize-homintvector!
                              (lambda (n) (make-s32vector n))
                              (lambda (v i n) (s32vector-set! v i n))
                              4
                              #t
                              len))
                            ((= type (u32vector-tag))
                             (deserialize-homintvector!
                              (lambda (n) (make-u32vector n))
                              (lambda (v i n) (u32vector-set! v i n))
                              4
                              #f
                              len))
                            ((= type (s64vector-tag))
                             (deserialize-homintvector!
                              (lambda (n) (make-s64vector n))
                              (lambda (v i n) (s64vector-set! v i n))
                              8
                              #t
                              len))
                            ((= type (u64vector-tag))
                             (deserialize-homintvector!
                              (lambda (n) (make-u64vector n))
                              (lambda (v i n) (u64vector-set! v i n))
                              8
                              #f
                              len))
                            ((= type (f32vector-tag))
                             (deserialize-homfloatvector!
                              (lambda (n) (make-f32vector n))
                              (lambda (v i n) (f32vector-set! v i n))
                              len
                              #t))
                            ((= type (f64vector-tag))
                             (deserialize-homfloatvector!
                              (lambda (n) (make-f64vector n))
                              (lambda (v i n) (f64vector-set! v i n))
                              len
                              #f))
                            (else
                             (error 9)))))

                   (else
                    (error 10))))

            ((>= x (exact-int-tag))
             (let ((lo (bitwise-and x #x0f)))
               (if (< lo #x0b)
                   lo
                   (let* ((len
                           (if (= lo #x0f)
                               (deserialize-nonneg-fixnum! 0 0)
                               (- #x0f lo)))
                          (n
                           (deserialize-exact-int-of-length! len)))
                     (if (= lo #x0e)
                         n
                         (begin
                           (alloc! n)
                           n))))))

            ((>= x (subprocedure-tag))
             (let ((subproc-id
                    (let ((id (bitwise-and x #x0f)))
                      (if (< id #x0f)
                          id
                          (deserialize-nonneg-fixnum! 0 0)))))
               (deserialize-subprocedure-with-id! subproc-id)))

            ((>= x (structure-tag))
             (deserialize-vector-like!
              (macro-subtype-structure)
              x))

            ((>= x (vector-tag))
             (deserialize-vector-like!
              (macro-subtype-vector)
              x))

            ((>= x (string-tag))
             (let ((obj (deserialize-string! x #x0f)))
               (alloc! obj)
               obj))

            (else ;; symbol-tag
             (let* ((name (deserialize-string! x #x0f))
                    (obj (string->symbol name)))
               (create-global-var-if-needed obj)
               (alloc! obj)
               obj)))))

  (define (deserialize!)
    (let* ((obj (deserialize-without-transform!))
           (transform (vector-ref state 4)))
      (transform obj)))

  (let ((obj (deserialize!)))
    (if (eof?)
        obj
        (error 11))))

(define-prim (u8vector->object
              u8vect
              #!optional
              (transform (macro-absent-obj)))
  (macro-force-vars (u8vect transform)
    (macro-check-u8vector u8vect 1 (u8vector->object u8vect transform)
      (if (eq? transform (macro-absent-obj))
          (##u8vector->object u8vect)
          (macro-check-procedure transform 2 (u8vector->object u8vect transform)
            (##u8vector->object u8vect transform))))))

;----------------------------------------------------------------------------

(define (##quasi-append . args) (##apply append args))
(define (##quasi-list . args) args)
(define (##quasi-cons x y) (##cons x y))

(##define-macro (define-runtime-macro pattern . rest)

  (define (form-size parms) ;; this definition must match the one in "_eval.scm"
    (let loop ((lst parms) (n 1))
      (cond ((pair? lst)
             (let ((parm (car lst)))
               (if (memq parm '(#!optional #!key #!rest))
                 (- n)
                 (loop (cdr lst)
                       (+ n 1)))))
            ((null? lst)
             n)
            (else
             (- n)))))

  (let ((src `(lambda ,(cdr pattern) ,@rest)))
    `(##top-cte-add-macro!
      ##interaction-cte
      ',(car pattern)
      (##make-macro-descr
       #f
       ',(form-size (cdr pattern))
       ,src
       #f))))

(##define-macro (define-macro* pattern . rest)
  `(begin
     (define-macro ,pattern ,@rest)
     (define-runtime-macro ,pattern ,@rest)))

(define (error . msgs)
  (for-each pretty-print msgs)
  (exit 1))

(define ##max-char 255)
(define ##max-fixnum 65535)

(define (##global-var? sym) #t)
;;;(define (##subprocedure-parent-name proc) 'toto)
(define (##system-version) 407002)


#;(begin
(define (##integer-length n)
  (if (or (##fx= n 0) (##fx= n -1))
      0
      (##fx+ 1 (##integer-length (##fxarithmetic-shift-right n 1)))))

(define (##bit-set? x y) (##fxodd? (##fxarithmetic-shift-right y x)))
)

       (define (##fifo->u8vector fifo start end)
         (let* ((len (##fxmax (##fx- end start) 0))
                (vect (##make-u8vector len)))
           (let loop ((elems (macro-fifo-next fifo))
                      (hi end)
                      (lo start)
                      (i 0))
             (if (##fx< lo hi)
                 (let* ((chunk
                         (macro-fifo-elem elems))
                        (chunk-len
                         (##u8vector-length chunk))
                        (n
                         (##fxmin (##fx- chunk-len lo)
                                  (##fx- hi lo))))
                   (##subu8vector-move! chunk lo (##fx+ lo n) vect i)
                   (loop (macro-fifo-next elems)
                         (##fx- hi chunk-len)
                         (##fx- (##fx+ lo n) chunk-len)
                         (##fx+ i n)))
                 vect))))

(define make-adder (lambda (x) (lambda (y) (##fx+ x y))))
(define foo (make-adder 42))

;(define foo (lambda (x) (##fx+ x x)))
;(define foo '(1 allo "foo"))
;(define foo 1234)

(define (serdes obj)
  (let* ((s (object->u8vector obj))
         (d (u8vector->object s)))
    d))

(define (check obj)
  (println (##eq? obj (serdes obj))))

(check  #f)
(check  #t)
(check  '())
(check  #!eof)
(check  #!void)
(check  (macro-absent-obj))
(check  #!unbound)
(check  #!unbound2)
(check  #!optional)
(check  #!key)
(check  #!rest)

#|
(define x (delay (begin (println 123) 456)))

(println "---------\n")
(force x)
(println "---------\n")
(force x)
(println "---------\n")
|#
       
(pretty-print foo)

(define sfoo (object->u8vector foo))

(pretty-print sfoo)

(define dfoo (u8vector->object sfoo))

(pretty-print dfoo)

(pretty-print (dfoo 10))

(pretty-print (list->string (map integer->char (u8vector->list sfoo))))
