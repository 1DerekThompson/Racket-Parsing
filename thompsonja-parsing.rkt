;;Derek Thompson
;; 2/25/15
;;Did work on with John so I sort of understand what is going on, definitely not as much as I should know.
#lang racket

(struct WAE ())
(struct id WAE (n))
(struct plus WAE (lhs rhs))
(struct minus WAE (lhs rhs))
(struct with WAE (id (n)lhs)rhs)

(define (parse sexp)
  (cond
    [(empty? sexp) (error "Never get here.")]
    [(symbol? sexp) (id sexp)] ;; this checks for the symbol of with.
    [(number? sexp) (num sexp)]
    [(equal? (first sexp) '+)
     (plus (parse (second sexp))
           (parse (third sexp)))]
    [(equal? (first sexp) '-)
     (minus (parse (second sexp))
            (parse (third sexp)))]
    [(equal? (first sexp) 'with)
     (with (parse (second sexp)) ;;setting up the with struct to do something.
           (parse (third sexp)))]))

(define (interp ae)
  (match ae
    [(struct num (n)) n]
    [(struct id (n))n]
    [(struct plus (left right))
     (+ (interp left) (interp right))]
    [(struct minus (left right))
     (- (interp left) (interp right))]
    [(struct with (left right))
     (let ([(first left) (second left)] right))]))

