#lang racket
(require  racket/date
          utils/stringUtilities)

(provide 
  get-current-date-string
  get-current-time-string
  get-current-date-time-string)

;; tested manually
(define (get-current-date-string [maybe-date #f])
  (let ([date (if (date? maybe-date) maybe-date (current-date))])
    (string-join 
      (map 
          (lambda (num) 
                  (string-front-pad-to-length (number->string num) "0" 2)) 
          (list (date-year date) (date-month date) (date-day date)))
      "-")))

;; tested manually
(define (get-current-time-string [maybe-date #f])
  (let ([date (if (date? maybe-date) maybe-date (current-date))])
    (string-join
      (map 
        (lambda (num) 
                (string-front-pad-to-length (number->string num) "0" 2))
        (list (date-hour date) (date-minute date) (date-second date)))
      ":")))

;; tested manually
(define (get-current-date-time-string [maybe-date #f])
  (let ([date (if (date? maybe-date) maybe-date (current-date))])
    (string-join  
      (list (get-current-date-string date)
            (get-current-time-string date)))))
