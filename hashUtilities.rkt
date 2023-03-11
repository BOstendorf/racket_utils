#lang racket

(provide hash-member)

(define (hash-member key hash)
  (hash-ref hash key #f))