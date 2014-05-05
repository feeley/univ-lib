;;;============================================================================

;;; File: "read.scm"

;;; Copyright (c) 2014 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;; Reader.

(define (read port)
  (let ((c (peek-ch-non-whitespace port)))
    (cond ((not (char? c))
           c)
          ((char=? c #\()
           (read-ch port) ;; skip #\(
           (read-list port))
          ((char=? c #\")
           (read-ch port) ;; skip #\"
           (list->string (read-string port)))
          ((char=? c #\')
           (read-ch port) ;; skip #\'
           (let ((datum (read port)))
             (list 'quote datum)))
          (else
           (read-ch port) ;; skip first char
           (let ((s (list->string (cons c (read-symbol port)))))
             (or (string->number s)
                 (string->symbol s)))))))

(define (read-list port)
  (let ((c (peek-ch-non-whitespace port)))
    (cond ((not (char? c))
           (scheme-error "missing closing parenthesis"))
          ((char=? c #\))
           (read-ch port) ;; skip #\)
           '())
          ((char=? c #\.)
           (read-ch port) ;; skip #\.
           (let ((datum (read port)))
             (let ((c2 (peek-ch-non-whitespace port)))
               (if (and (char? c2)
                        (char=? c2 #\)))
                   (begin
                     (read-ch port) ;; skip #\)
                     datum)
                   (scheme-error "missing closing parenthesis")))))
          (else
           (let ((first (read port)))
             (let ((rest (read-list port)))
               (cons first rest)))))))

(define (read-string port)
  (let ((c (read-ch port)))
    (if (not (char? c))
        (scheme-error "unterminated string")
        (if (char=? c #\")
            '()
            (if (char=? c #\\)
                (let ((c2 (read-ch port)))
                  (cons c2 (read-string port)))
                (cons c (read-string port)))))))

(define (read-symbol port)
  (let ((c (peek-ch port)))
    (if (or (not (char? c))
            (delimiter? c))
        '()
        (begin
          (read-ch port)
          (cons c (read-symbol port))))))

(define (delimiter? c)
  (or (char=? c #\()
      (char=? c #\))
      (char=? c #\')
      (char=? c #\")
      (char<=? c #\space)))

(define (peek-ch-non-whitespace port)
  (let ((c (peek-ch port)))
    (cond ((not (char? c))
           c)
          ((char<=? c #\space)
           (read-ch port)
           (peek-ch-non-whitespace port))
          ((char=? c #\;)
           (let loop ()
             (let ((c2 (read-ch port)))
               (if (and (char? c2)
                        (not (char=? c2 #\newline)))
                   (loop)
                   (peek-ch-non-whitespace port)))))
          (else
           c))))

(define (peek-ch port)
  (##peek-char port))

(define (read-ch port)
  (##read-char port))

(define (string->object str)
  (let ((port (##open-string)))
    (##write-string str port)
    (##write-eof port)
    (read port)))

;;;============================================================================
