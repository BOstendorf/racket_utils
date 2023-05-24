#lang racket
(require  utils/io-utilities
          utils/date-timeUtilities)

(provide
  log-path
  set-log-file-path
  log)

(define log-path "/tmp/racket_logs/log.txt")

(define (set-log-file-path path)
  (let ([resolved-path (resolve-path path)])
    (if (equal? 'no-path-or-path-string-provided resolved-path)
        (raise-argument-error 'path-does-not-resolve "(or (? path?) (? path-string?))" path)
        (set! log-path path))))

(define (write-log-line line file-path)
  (begin 
    (mkdir (path-only file-path))
    (line-writer  file-path 
                  (if (list? line) 
                      (cons (string-append  (get-current-date-time-string) 
                                            " - ") 
                            line) 
                      (list (string-append  (get-current-date-time-string) 
                                            " - ") 
                            line)))))


(define (log message [path #f])
  (let ([resolved-path (resolve-path (if path path log-path))])
    (if (equal? resolved-path 'no-path-or-path-string-provided)
        (raise-argument-error 'path-does-not-resolve "(or (? path?) (? path-string?))" path)
        (cond 
          [(string? message) (write-log-line message resolved-path)]
          [(list? message) 
            (for-each (lambda (elem)
                              (and  (string? elem)
                                    (write-log-line elem resolved-path))))]
          [else (raise-argument-error 'invalid-log-message 
                                      "(or (? string?) (list (? string?) ...))" 
                                      message)]))))
