#lang racket

(require  rackunit
          rackunit/text-ui)

(provide string-trim-until)

;; tested
(define (string-trim-until str until)
  (cond 
    [(string-prefix? str until) str]
    [(string-contains? str until) (string-trim-until (substring str 1) until)]
    [else ""])
  )


;;; ---------------------------------------------
;;; test cases - invoke using thunk execute-tests
;;; ---------------------------------------------

(define string-trim-until-tests
  (test-suite "tests for string-trim-until get suffix of string starting with until pattern"
    (test-equal? "until pattern not present in string"
          (string-trim-until "someTestString" "seperator")
          "")
    (test-equal? "until pattern is prefix of string"
          (string-trim-until "sepSomeString" "sep")
          "sepSomeString")
    (test-equal? "until pattern is part of string multiple times"
          (string-trim-until "some.string.with.dots." ".")
          ".string.with.dots.")
    (test-equal? "string equals until pattern"
          (string-trim-until "pattern" "pattern")
          "pattern")
  ))

(define (execute-tests)
  (run-tests string-trim-until-tests)
)