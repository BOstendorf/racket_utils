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


(define (string-replace** str patterns replacement . further)
  (cond [(empty? further) str]
        [(empty? (cdr further)) 
          (raise-argument-error 'expected-pattern-and-replacement-to-be-provided-as-argument
                                "(list  (or string? 
                                            (list string? ...))
                                        string?) ..."
                                further)]
        [(string? patterns) (string-replace*  (string-replace str patterns replacement)
                                              (first further)
                                              (second further)
                                              (cddr further))]
        [(empty? patterns) (string-replace* str 
                                            (first further)
                                            (second further))
                                            (cddr further)]
        [(list? patterns) (string-replace*  (string-replace str (car patterns) replacement)
                                            (cdr patterns)
                                            replacement
                                            further)]))

(define/match (string-replace* str patterns replacement . further)
  [((? string?) (? string?) (? string?) '()) (string-replace str patterns replacement)]
  [((? string?) (? string?) (? string?) (list-rest pat repl furth)) (string-replace* str pat repl furth)])

;;; ---------------------------------------------
;;; test cases - invoke using thunk execute-tests
;;; ---------------------------------------------

(define string-replace*-tests
  (test-suite "tests for string-replace* :: should replace all patterns provided"
    (test-equal? 
          "same behaviour as string-replace if only one source, one pattern and one replacement string are provided as parameters"
          (string-replace* "string-mit-Minus" "-" "+")
          "string+mit+Minus")
    (test-equal? 
          "provide a second pattern and replacement"
          (string-replace* "string-mit-Minus" "-" "+" "Minus" "Plus")
          "string+mit+Plus")
  ))

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
  (run-tests string-replace*-tests)
)