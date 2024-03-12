#lang racket

(require
  lens
  relation
  racket/control)

(provide
 view-curried
 transform-curried
 set-curried
 search-lens)

(define/f (view-curried lens target)
  (lens-view lens target))

(define/f (transform-curried lens transformer target)
  (lens-transform lens target transformer))

(define/f (set-curried lens new-view target)
  (lens-set lens target new-view))

(define (search-lens old-val)
  (make-lens
   (curry findf
     (curry equal? old-val))
   (λ (ls new-val)
     (stream->list
      (map
       (λ (el)
         (if (equal? el old-val)
             new-val
             el))
       ls)))))

(define parameter-lens
  (make-lens
   call
   (λ (par new-val)
     (make-parameter new-val))))

(define/f (view-parameter par)
  (par))

(define/f (set-parameter val par)
  (parameterize ([par val]) par))

(define location
  (make-parameter "here"))

(define as
  (list
   (cons 1 'a)
   (cons 2 'b)
   (cons 3 'c)))

(define (map/cc ls)
  (call/comp
   (λ (k)
     (abort
      (map k ls)))))
       

(define thunk-lens
  (make-lens
   call
   (λ (thnk val)
     (thunk val))))
     