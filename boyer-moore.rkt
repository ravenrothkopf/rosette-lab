#lang rosette

; Boyer-moore majority voting algorithm:
; Initialize an element m and a counter i with i = 0
; For each element x of the input sequence:
; If i = 0, then assign m = x and i = 1
; else if m = x, then assign i = i + 1
; else assign i = i − 1
; Return m

(define (boyer-moore xs)
  (define m (car xs)) ; define m as an element of the xs list
  (define i 0)
  (for ([x xs]) ; for elements in list, check conditions
    (cond [(= i 0) (set! m x) (set! i 1)]
          [(= m x) (set! i (i + 1))]
          [else (set! i (i - 1))]))
  m) ; return m

(define (check n [bw #f])
  (current-bitwidth bw)
  (define-symbolic* xs integer?)
  (define-symbolic* m integer?) ; the majority must be more than half the total
  (verify (assert (= m (boyer-moore xs)))))

(time (check 100)) 