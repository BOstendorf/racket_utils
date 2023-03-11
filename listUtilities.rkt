#lang racket

(provide  member-of-some?
          not-member
          not-member-of-some?
          filter->append
          remove-excluded
          cons-non-member
          member*)

;; tested
(define (member-of-some? elem potential-lists)
  (foldl  (lambda  (potential-list is-member-flag)
                  (or is-member-flag (member elem potential-list)))
                  #f
          potential-lists))

;; tested
(define not-member-of-some?
  (case-lambda
    [(potential-lists) (curryr not-member-of-some? potential-lists)]
    [(elem potential-lists) (not (member-of-some? elem potential-lists))]))

;; tested
(define (filter->append filter-pred list-to-filter end-list)
  (foldl  (lambda   (elem-to-check akku-lst)
                    (if (filter-pred elem-to-check)
                        (cons elem-to-check akku-lst)
                        akku-lst))
          end-list
          (reverse list-to-filter)))

;; tested
(define (not-member elem lst)
  (not (member elem lst)))

;; tested
(define (remove-excluded lst excluded-elems)
  (filter (curryr not-member excluded-elems) lst))

;; tested
(define (cons-non-member elem lst)
  (if (member elem lst)
      lst
      (cons elem lst)))

;; tested
(define/match (member* members? lst [flag 'unset])
  [(_ _ 'unset) (member* members? lst #t)]
  [(_ _ #f) #f]
  [('() ls (not #f)) #t]
  [((list x) ls (not #f)) (and (member x ls) #t)]
  [((cons x r) ls (not #f)) (member* r ls (and (member x ls) #t))]
  [(_ _ _) #f])