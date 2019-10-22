#lang racket

(define LAMBDA (string->symbol "\u03BB"))

;bind a b into a!b
(define (bind-var a b)
  (string->symbol (string-append (symbol->string a) "!" (symbol->string b)))
  )

;compare simple subexpressions
(define (compare-simple a b)
  (cond
    [(equal? a b ) a]
    [(and (equal? a #t) (equal? b #f)) '%]
    [(and (equal? a #f) (equal? b #t)) '(not %)]
    [#t (append '(if %) (list a b))]
    );end cond
  )

;try combine
;compare lists
(define (same-head a b)
  (cond
    [(equal? (car a) 'lambda) (compare-lambda a b)]
    [(equal? (car a) 'λ) (compare-lambda a b)]
    [(equal? (car a) 'quote) (compare-simple a b)]
    [#t (compare-list a b)]
  )
  )

;recursive function
(define (compare-list a b)
  (cond
     [(and (not(equal? '() a)) (not (equal? '() b))) (cons (expr-compare (car a) (car b)) (compare-list (cdr a) (cdr b)))]
     [#t '()]
    );end cond
  )

;function for different head
(define (diff-head a b)
  (cond
    [(or (and (equal? 'lambda (car a)) (equal? LAMBDA (car b))) (and (equal? 'lambda (car b)) (equal? LAMBDA (car a))))(compare-lambda a b)]
    [(or (equal? 'lambda (car a)) (equal? 'lambda (car b)) (equal? LAMBDA (car a)) (equal? LAMBDA (car a))) (compare-simple a b)]
    [(or (equal? 'quote (car a)) (equal? 'quote (car b))) (compare-simple a b)]
    [(or (equal? 'if (car a)) (equal? 'if (car b))) (compare-simple a b)]
    [#t (compare-list a b)]
  )
  )

;part 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (expr-compare a b)
  (if (and (list? a) (list? b))
      (cond
        [(equal? (length a) (length b)) (if (equal? (car a) (car b)) (same-head a b) (diff-head a b))]
        [#t (compare-simple a b)]
        );end cond
      (compare-simple a b)
      );end if 
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;form a dictionary
(define (helper-form-dict a b c)
  (cond
    [(equal? a '()) c]
    [#t (cond
          [(equal? (car a) (car b)) (helper-form-dict (cdr a) (cdr b) (cons (car a) c))]
          [#t (helper-form-dict (cdr a) (cdr b) (cons (bind-var (car a) (car b)) c))]
     )];end cond
    );end cond
  )

(define (form-dict a b)
  (list a b (reverse (helper-form-dict a b '() )))
  )

;helper function, check if an atom is a pair
(define (ispair atom)
  (and (not (list? atom)) (pair? atom))
  )

;helper function, find the first occurence of an element
(define (find-pos lis ele)
  (cond
    [(equal? lis '()) 0]
    [(equal? (car lis) ele) 0]
    [#t (+ 1 (find-pos (cdr lis) ele))]
    )
  )

;change variables to binded form
(define (change-var ini fin clause)
  (cond
    [(ispair clause) (cons (change-var ini fin (car clause)) (change-var ini fin (cdr clause)))]
    [(not (list? clause)) (if (member clause ini) (list-ref fin (find-pos ini clause)) clause)]
    [(equal? clause '()) '()]
    [#t (cond
          [(list? (car clause)) (if (or (equal? (caar clause) 'lambda) (equal? (caar clause) LAMBDA))
                                    (cons (car clause) (change-var ini fin (cdr clause)))
                                    (cons (change-var ini fin (car clause)) (change-var ini fin (cdr clause))))]
          [#t (cons (change-var ini fin (car clause)) (change-var ini fin (cdr clause)))]
     )];end cond
    );end cond
  )

;compare lambda subexpressions
(define (compare-lambda a b)
  (let ([Lform (if (or (equal? LAMBDA (car a)) (equal? LAMBDA (car b))) LAMBDA 'lambda)])
    (cond
      [(equal? (cadr a) (cadr b)) (cons Lform (expr-compare (cdr a) (cdr b)))] ;same declaration
      [#t (cond
            [(and (list? (cadr a)) (list? (cadr b))) (cond
                                                       [(not (equal? (length (cadr a)) (length (cadr b)))) (compare-simple a b)] ;different length
                                                       [#t (let ([dict (form-dict (cadr a) (cadr b))])
                                                             (let ([clause_a (change-var (car dict) (caddr dict) (caddr a))]
                                                                   [clause_b (change-var (cadr dict) (caddr dict) (caddr b))])
                                                              (list Lform (caddr dict) (expr-compare clause_a clause_b))
                                                              );end let
                                                        )];end let
             )];end cond
            [(and (and (not (list? (cadr a))) (not (ispair (cadr a)))) (and (not (list? (cadr b))) (not (ispair (cadr b)))))
             (let ([dict (form-dict (list (cadr a)) (list (cadr b)))])
               (let ([clause_a (change-var (car dict) (caddr dict) (caddr a))]
                     [clause_b (change-var (cadr dict) (caddr dict) (caddr b))])
                 (list Lform (caaddr dict) (expr-compare clause_a clause_b))
                 );end let
               )]
            [(and (ispair (cadr a)) (ispair (cadr b)))
             (let ([dict (form-dict (list (caadr a) (cdadr a)) (list (caadr b) (cdadr b)))])
               (let ([clause_a (change-var (car dict) (caddr dict) (caddr a))]
                     [clause_b (change-var (cadr dict) (caddr dict) (caddr b))])
                 (list Lform (cons (caaddr dict) (car (cdaddr dict))) (expr-compare clause_a clause_b))
                 );end let
               )]
            [#t (compare-simple a b)]
       )];end cond
      );end cond
    );end let
  )

;part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (test-expr-compare x y)
  (cond
    [(and (equal? (eval (list 'let '((% #t)) (expr-compare x y))) (eval x))
    (equal? (eval (list 'let '((% #f)) (expr-compare x y))) (eval y))) #t]
    [#t #f]
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;part 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define test-expr-x 
  '((lambda (lis) (+ (car lis) (cadr lis))) 
    ((lambda (a b) (if (number? a) (list a b) (list b a))) (car (quote (1 2))) 2)
    )
)

(define test-expr-y
  '((λ (sis) (+ (car sis) (cadr sis))) 
    ((lambda (b a) (if (number? a) (list a b) (list b a))) (abs (car (list 3 2))) 2)
    )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
