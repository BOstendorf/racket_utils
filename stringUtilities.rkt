#lang racket

(require  rackunit
          rackunit/text-ui)

(provide  string-trim-until
          string-replace*
          string-prefix*
          string-front-pad-to-length
          string-contains-in-order?
          single-digit-string?
          number-string?
          execute-tests)

(define (string-front-pad-to-length str padding-char desired-length)
  (cond 
    [(>= (string-length str) 
          desired-length) str]
    [(or  (not (string? padding-char))
          (not (= (string-length padding-char) 
                  1)))
      (raise-argument-error 'invalid-padding-char 
                            "(and (? string?) (app string-length 1))" 
                            padding-char)]
    [else (string-front-pad-to-length (string-append padding-char str) 
                                      padding-char 
                                      desired-length)]))

;; tested
(define (string-trim-until str until)
  (cond 
    [(string-prefix? str until) str]
    [(string-contains? str until) (string-trim-until (substring str 1) until)]
    [else ""])
  )

;; tested
(define (string-prefix* str prefixes)
  (cond [(empty? prefixes) #f]
        [(string-prefix? str (car prefixes)) (car prefixes)]
        [else (string-prefix* str (cdr prefixes))]))

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

;; tested
(define (single-digit-string? str)
  (and (string? str)
       (equal? (string-length str)
               1)
       (member str '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0"))))

;; tested
(define (number-string? str)
  (define (loop lst)
    (cond 
      [(empty? lst) #t]
      [(equal? (car lst) "") (loop (cdr lst))]
      [(single-digit-string? (car lst)) (loop (cdr lst))]
      [else #f]
      ))
  (cond [(not (string? str)) #f]
        [else (loop (string-split str ""))])
  )


(define (string-contains-in-order? str expected-substrings accepted-seperators)
  (cond [(empty? expected-substrings) str]
        [(equal? str "") #f]
        [(string-prefix? 
           str 
           (car expected-substrings)) 
         (string-contains-in-order? (string-replace str (car expected-substrings) "" #:all? #f)
                                    (cdr expected-substrings)
                                    accepted-seperators)]
        [else (let ([seperator-prefix (string-prefix* str accepted-seperators)])
                (if 
                  seperator-prefix
                  (string-contains-in-order? (string-replace str seperator-prefix "" #:all? #f)
                                             expected-substrings
                                             accepted-seperators)
                  #f))]))

;;; ---------------------------------------------
;;; test cases - invoke using thunk execute-tests
;;; ---------------------------------------------

(define string-contains-in-order?-tests
  (test-suite 
    "tests for string-contains-in-order?"
    (test-suite 
      "positive cases"
      (test-case "testing strings without seperators"
                 (check-equal? (string-contains-in-order?
                                 "hier ist ein string"
                                 '("hier " "ist")
                                 '())
                               " ein string")
                 (check-equal? (string-contains-in-order?
                                 "hier ist ein string"
                                 '()
                                 '())
                               "hier ist ein string")
                 (check-equal? (string-contains-in-order?
                                 "dis string"
                                 '("d" "is" " ")
                                 '())
                               "string")
                 )
      (test-case "testing strings with seperators"
                 (check-equal? (string-contains-in-order?
                                 "hier ist ein string"
                                 '("hier " "string")
                                 '(" " "ein" "ist"))
                               "")
                 (check-equal? (string-contains-in-order?
                                 "hier ist ein string"
                                 '("hier" "ein")
                                 '(" ist "))
                               " string")
                 (check-equal? (string-contains-in-order?
                                 "hier ist ein string"
                                 '("ist" "ein")
                                 '("hier" " "))
                               " string")))
    (test-case
      "negative cases"
      (check-false (string-contains-in-order?
                     "hier ist ein"
                     '(" " "hier")
                     '(" ")))
      (check-false (string-contains-in-order?
                     "hier ist ein"
                     '("ein" "hier")
                     '(" ist "))))))

(define number-string?-tests
  (test-suite 
    "tests for number-string?"
    (test-suite 
      "negative cases"
      (test-case "testing strings that are not number strings"
                 
                 (check-false (number-string? "string"))
                 (check-false (number-string? " 123"))
                 (check-false (number-string? "0.5.3"))
                 (check-false (number-string? "0.5"))
                 (check-false (number-string? "123 string"))
                 )
      (test-case "testing non string elements"
                 (check-false (number-string? 'sym))
                 (check-false (number-string? '(1 2 3)))
                 (check-false (number-string? #f))
                 (check-false (number-string? #t))
                 (check-false (number-string? 123))
                 (check-false (number-string? (hash)))
                 (check-false (number-string? '("1" "23")))
                 ))
    (test-suite
      "positive cases"
      (test-case "strings that represent numbers"
                 (check-true (number-string? "123") "test for 123")
                 (check-true (number-string? "04") "test for 04")
                 (check-true (number-string? "15646") "test for 15646")
                 (check-true (number-string? "0") "test for 0")
                 (check-true (number-string? "0104502") "test for 0104502")
                 ))))


(define single-digit-string?-tests
  (test-suite 
    "tests for single-digit-string?"
    (test-true "test for all correct strings"
               (foldl (lambda (str akku)
                        (and (single-digit-string? str)
                             akku))
                      #t
                      '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0")))
    (test-case "negative cases"
               (check-false (single-digit-string? "10"))
               (check-false (single-digit-string? "a"))
               (check-false (single-digit-string? ""))
               (check-false (single-digit-string? "some other string"))
               (check-false (single-digit-string? '()))
               (check-false (single-digit-string? 'sym))
               (check-false (single-digit-string? 45))
               (check-false (single-digit-string? #f))
               (check-false (single-digit-string? #t))
               )))

(define string-prefix*-tests
  (test-suite "tests for string-prefix*"
    (test-not-false 
          "String has only prefix present in arguments"
          (string-prefix* "testString" '("test")))
    (test-not-false 
          "String has first prefix present in arguments"
          (string-prefix* "testString" '("test" "someother")))
    (test-not-false 
          "String has second prefix present in arguments"
          (string-prefix* "testString" '("someother" "test")))
    (test-equal?
      "empty string as expected prefix"
      (string-prefix* "testString" '("" "String"))
      "")
    (test-false
          "No prefixes supplied as argument"
          (string-prefix* "testString" '()))
    (test-false
          "None of supplied prefixes is prefix of string"
          (string-prefix* "testString" '("some" "prefix")))
  ))

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
  (run-tests string-prefix*-tests)
  (run-tests single-digit-string?-tests)
  (run-tests number-string?-tests)
  (run-tests string-contains-in-order?-tests)
)
