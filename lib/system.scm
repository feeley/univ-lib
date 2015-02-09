;;;============================================================================

;;; File: "system.scm"

;;; Copyright (c) 1994-2014 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;-----------------------------------------------------------------------------

;;; Object equality.

(define-prim (##eqv? obj1 obj2)
  (if (##flonum? obj1)
      (and (##flonum? obj2)
           (##fl= obj1 obj2)) ;; TODO: test that bits are equal
      (##eq? obj1 obj2)))

(define-prim (eqv? obj1 obj2)
  (macro-force-vars (obj1 obj2)
    (let ()
      (##declare (generic)) ;; avoid fixnum specific ##eqv?
      (##eqv? obj1 obj2))))

#;
(define-prim (##eq? obj1 obj2))

(define-prim (eq? obj1 obj2)
  (macro-force-vars (obj1 obj2)
    (##eq? obj1 obj2)))

(define-prim (##equal? obj1 obj2)

  (define (equal obj1 obj2)
    (macro-force-vars (obj1 obj2)
      (cond ((##pair? obj1)
             (and (##pair? obj2)
                  (equal (##car obj1) (##car obj2))
                  (equal (##cdr obj1) (##cdr obj2))))
            ((##string? obj1)
             (and (##string? obj2)
                  (##string=? obj1 obj2)))
            ((##vector? obj1)
             (and (##vector? obj2)
                  (let ((len (##vector-length obj1)))
                    (and (##fx= (##vector-length obj2) len)
                         (let loop ((i (##fx- len 1)))
                           (or (##fx< i 0)
                               (and (equal (##vector-ref obj1 i)
                                           (##vector-ref obj2 i))
                                    (loop (##fx- i 1)))))))))
            (else
             (let ()
               (##declare (generic)) ;; avoid fixnum specific ##eqv?
               (##eqv? obj1 obj2))))))

  (equal obj1 obj2))

(define-prim (equal? obj1 obj2)
  (##equal? obj1 obj2))

;;;============================================================================
