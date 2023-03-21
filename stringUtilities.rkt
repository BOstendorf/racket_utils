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

;; tested
(define (string-replace* str patterns replacement [further '()])
  (letrec 
    ([process-further 
            (lambda (str further) 
                    (cond [(empty? further) str]
                          [(empty? (cdr further)) 
                              (raise-argument-error 'expected-pattern-and-replacement-to-be-provided-as-argument
                                                    "(list  (or string? 
                                                            (list string? ...))
                                                            string?) ..."
                                                    further)]
                          [else (process-replacement  str 
                                                      (first further)
                                                      (second further)
                                                      (cddr further))]))]
      [process-replacement 
            (lambda (str patterns replacement further)
                    (cond [(string? patterns) (process-further  (string-replace str 
                                                                                patterns 
                                                                                replacement)
                                                                further)]
                          [(list? patterns) (process-further (foldl (lambda (pat str)
                                                                            (string-replace str pat replacement))
                                                                    str
                                                                    patterns)
                                                              further)]))])
    (process-replacement str patterns replacement further)))

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
          (string-replace* "string-mit-Minus" "-" "+" '("Minus" "Plus"))
          "string+mit+Plus")
    (test-equal?
          "provide a list of patterns with one replacement"
          (string-replace* "string-mit-Minus" '("-" "Minus") "+")
          "string+mit++")
    (test-equal? 
          "provide a list of patterns with one replacement and further pattern string with replacement"
          (string-replace* "string-mit-Minus" '("-" "mit") "+" '("Minus" "Plus"))
          "string+++Plus")
    (test-equal?
          "provide a list of patterns with replacement and further list of patterns with replacement"
          (string-replace* "string-mit-Minus" '("-" "mit") "+" '(("Minus" "string") "Plus"))
          "Plus+++Plus")
    (test-equal?
          "pattern string and further list of patterns"
          (string-replace* "string-mit-Minus" "mit" "+" '(("Minus" "string") "Plus"))
          "Plus-+-Plus")
    (test-equal?
          "pattern list and multiple further pattern strings"
          (string-replace* "string-mit-Minus" "mit" "+" '("-" "+" "Minus" "Plus"))
          "string+++Plus")
    (test-equal?
          "pattern list and multiple further pattern lists"
          (string-replace* "string-mit-Minus" "mit" "+" '("-" "+" ("string" "Minus") "Plus"))
          "Plus+++Plus")
    (test-equal?
          "pattern list and multiple further pattern lists modifying earlier replacement"
          (string-replace* "string-mit-Minus" "mit" "+" 
                                              '("-" "+" 
                                                ("string" "Minus") "Plus" 
                                                ("Plus+" "+Plus") "+"))
          "+++")
    (test-equal?
          "no pattern to replace present"
          (string-replace* "string-mit-Minus" "ohne" "+" '(("Plus" "+") "Minus"))
          "string-mit-Minus")
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