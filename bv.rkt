#lang rosette

; this implementation is buggy, need to generate a counter example (verify), and repair the function (synthesize)
(define (abs-spec x)
  (if (bvslt x (bv 0 32))
      (bvneg x)
      x))

(define (abs-impl x) 
  (let* ([o1 (bvashr x (bv 31 32))]
         [o2 (bvadd x o1)]
         [o3 (bvsub o2 o1)])
    o3))

; zero, positive, negative ...
(assert (equal? (abs-impl (bv #x00000000 32)) 
                  (abs-spec (bv #x00000000 32))))
(assert (equal? (abs-impl (bv #x00000003 32)) 
                  (abs-spec (bv #x00000003 32))))
(assert (equal? (abs-impl (bv #x80000000 32)) 
                  (abs-spec (bv #x80000000 32))))

; Q1
(define-symbolic b (bitvector 32))
(define cex (verify (assert (equal? (abs-impl b) (abs-spec b) )))) ; parenthesis matter? (abs-impl (b)) doesn't give you the model
(evaluate b cex) ; need evaluate to get the value found with verify, takes a value and satisfiable solution

; counter example:
; (bv #x8404c7c0 32)

; Q2
; (require rosette/query/debug rosette/lib/render)
; define/debug is depricated now!

; Q3
(require rosette/lib/synthax)

(define (abs-impl-3 x) 
  (let* ([o1 (bvashr x (bv 31 32))]
         [o2 ((choose bvadd bvashr bvsub bvlshr bvxor bvshl bvor) x o1)] ; be careful of parentheses with choose
         [o3 ((choose bvadd bvashr bvsub bvlshr bvxor bvshl bvor) o2 o1)])
    o3))

(print-forms
   (synthesize  
    #:forall b
    #:guarantee (assert (equal? (abs-impl-3 b) (abs-spec b) ))))

; repaired program:
; (define (abs-impl-3 x)
;  (let* ((o1 (bvashr x (bv 31 32))) (o2 (bvadd x o1)) (o3 (bvxor o2 o1))) o3))








