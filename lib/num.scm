;;;============================================================================

;;; File: "num.scm"

;;; Copyright (c) 1994-2014 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;; Numbers.

(define (##complex? obj)
  (or (##fixnum? obj)
      (##flonum? obj)))

(define (##number? obj)
  (or (##fixnum? obj)
      (##flonum? obj)))

(define (number? obj)
  (##number? obj))

(define (+ x y)
  (if (and (##fixnum? x) (##fixnum? y))
      (##fx+ x y)
      (##fl+ (if (##fixnum? x) (##fl<-fx x) x)
             (if (##fixnum? y) (##fl<-fx y) y))))

(define (- x #!optional y)
  (if y
      (if (and (##fixnum? x) (##fixnum? y))
          (##fx- x y)
          (##fl- (if (##fixnum? x) (##fl<-fx x) x)
                 (if (##fixnum? y) (##fl<-fx y) y)))
      (if (##fixnum? x)
          (##fx- x)
          (##fl- x))))

(define (* x y)
  (if (and (##fixnum? x) (##fixnum? y))
      (##fx* x y)
      (##fl* (if (##fixnum? x) (##fl<-fx x) x)
             (if (##fixnum? y) (##fl<-fx y) y))))

(define (/ x y)
  (##fl/ (if (##fixnum? x) (##fl<-fx x) x)
         (if (##fixnum? y) (##fl<-fx y) y)))

(define (quotient x y)
  (##fxquotient x y))

(define (remainder x y)
  (##fxremainder x y))

(define (modulo x y)
  (##fxmodulo x y))

(define (= x y)
  (if (and (##fixnum? x) (##fixnum? y))
      (##fx= x y)
      (##fl= (if (##fixnum? x) (##fl<-fx x) x)
             (if (##fixnum? y) (##fl<-fx y) y))))

(define (< x y)
  (if (and (##fixnum? x) (##fixnum? y))
      (##fx< x y)
      (##fl< (if (##fixnum? x) (##fl<-fx x) x)
             (if (##fixnum? y) (##fl<-fx y) y))))

(define (<= x y)
  (if (and (##fixnum? x) (##fixnum? y))
      (##fx<= x y)
      (##fl<= (if (##fixnum? x) (##fl<-fx x) x)
              (if (##fixnum? y) (##fl<-fx y) y))))

(define (> x y)
  (if (and (##fixnum? x) (##fixnum? y))
      (##fx> x y)
      (##fl> (if (##fixnum? x) (##fl<-fx x) x)
             (if (##fixnum? y) (##fl<-fx y) y))))

(define (>= x y)
  (if (and (##fixnum? x) (##fixnum? y))
      (##fx>= x y)
      (##fl>= (if (##fixnum? x) (##fl<-fx x) x)
              (if (##fixnum? y) (##fl<-fx y) y))))

;;-----------------------------------------------------------------------------

(define (number->string n)

  (define (fixnum->string n)

    (define (digits offset i n)
      (let* ((n/10
              (##fxquotient n 10))
             (str
              (if (##fx= n/10 0)
                  (make-string (##fx+ offset i))
                  (digits offset (##fx+ i 1) n/10))))
        (##string-set!
         str
         (##fx- (##string-length str) i)
         (##integer->char (##fx- 48 (##fxremainder n 10))))
        str))

    (if (##fx< n 0)
        (let ((str (digits 1 1 n)))
          (##string-set! str 0 #\-)
          str)
        (digits 0 1 (##fx- n))))

  (define (flonum->string n)
    "123.456") ;; Improve!

  (cond ((##fixnum? n)
         (fixnum->string n))
        ((##flonum? n)
         (flonum->string n))
        (else
         "error")))

(define (string->number str)

  (define (parse-num neg? at-least-one-digit? i n)
    (if (##fx< i (##string-length str))
        (let ((d (##char->integer (##string-ref str i))))
          (if (and (##fx>= d 48) (##fx<= d 57))
              (parse-num neg? #t (##fx+ i 1) (##fx- (##fx* n 10) (##fx- d 48)))
              #f))
        (if at-least-one-digit?
            (if neg? n (##fx- 0 n))
            #f)))

  (if (##fx> (##string-length str) 0)
      (let ((c (##string-ref str 0)))
        (cond ((##char=? c #\+)
               (parse-num #f #f 1 0))
              ((##char=? c #\-)
               (parse-num #t #f 1 0))
              (else
               (parse-num #f #f 0 0))))
      #f))

;;;============================================================================
