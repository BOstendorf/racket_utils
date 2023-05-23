#lang racket
(require utils/io-utilities)

(provide
  log-path
  set-log-file-path)

(define log-path "/tmp/rachet_logs/log.txt")

(define (set-log-file-path path)
  (let ([resolved-path (resolve-path path)])
    (if (equal? 'no-path-or-path-string-provided resolved-path)
        (raise-argument-error 'path-does-not-resolve "(or (? path?) (? path-string?))" path)
        (set! log-path path))))

(define (write-log-line line path)
  'todo)


(define (log message [path #f])
  (let ([resolved-path (resolve-path (if path path log-path))])
    (if (equal? resolved-path 'no-path-or-path-string-provided)
        (raise-argument-error 'path-does-not-resolve "(or (? path?) (? path-string?))" path)
        (cond 
          [(string? message) (write-log-line message resolved-path)]
          [(list? message) 'todo])))
  (match (list message path)
    [(list  (and  msg-string 
                  (? string?))
            #f) (write-log-line msg-string log-path)]
    [(list  (and  msg-string
                  (? string?))
            (? (lambda  (path) 
                        (equal? (resolve-path path) 
                                'no-path-or-path-string-provided))))
        (raise-argument-error 'path-does-not-resolve "(or (? path?) (? path-string?))" path)]
    [(list  (and  msg-string
                  (? string?))
            (? (lambda  (path) 
                        (not (equal?  (resolve-path path) 
                                      'no-path-or-path-string-provided)))))
        (write-log-line msg-string)]))