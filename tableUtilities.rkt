#lang racket
(require  rackunit
          rackunit/text-ui)

(provide  table-by-columns
          table-by-rows
          table->table-by-columns
          table->table-by-rows
          )

;;tested
(struct table-by-rows (table-rows)
  #:guard (lambda (rows name)
                  (unless (and (list? rows) (list? (car rows)))
                    (error "table-rows have to be provided as list of lists representing rows"))))

;;tested
(struct table-by-columns (table-columns)
  #:guard (lambda (rows name)
                  (unless (and (list? rows) (list? (car rows)))
                    (error "table-rows have to be provided as list of lists representing columns"))))

;; tested
(define/match (table->table-by-columns table)
  [((table-by-rows rows)) (table-by-columns (transpose rows))]
  [((table-by-columns columns)) (table-by-columns columns)]
  [(_) 'no-table-given])

;; tested
(define/match (table->table-by-rows table)
  [((table-by-columns columns)) (table-by-rows (transpose columns))]
  [((table-by-rows rows)) (table-by-rows rows)]
  [(_) 'no-table-given]
)

;; tested
(define/match (transpose rows/columns)
  [((list (list ))) '(())]
  [((list (app list? #f) ...)) 'no-nested-list-given->could-be-row-or-list]
  [((list (list _ ...) ...)) (apply map list rows/columns)]
  [(_) 'no-rows-or-columns-given]
)


;;; ---------------------------------------------
;;; test cases - invoke using thunk execute-tests
;;; ---------------------------------------------

(define transpose-tests
  (test-suite "tests for convertion from table-by-rows->table-by-columns and reverse"
    (let* (
      [nxm '( (col1-row1 col2-row1 col3-row1 col4-row1)
              (col1-row2 col2-row2 col3-row2 col4-row2)
              (col1-row3 col2-row3 col3-row3 col4-row3))]
      [0x0 '(())]
      [1x1 '((1x1))]
      [1xm '((col1 col2 col3 col4))]
      [nx1 '((col1) (col2) (col3) (col4))]
      [t-1x1 '((1x1))]
      [t-1xm '((col1) (col2) (col3) (col4))]
      [t-nx1 '((col1 col2 col3 col4))]
      [t-nxm '( (col1-row1 col1-row2 col1-row3)
                (col2-row1 col2-row2 col2-row3)
                (col3-row1 col3-row2 col3-row3)
                (col4-row1 col4-row2 col4-row3))])
    (test-case "tests for the transpose function"
      (test-equal? "empty rows or columns are returned as is"
          (transpose 0x0)
          0x0)
      (test-equal? "transpose 1x1 rows/columns"
          (transpose 1x1)
          t-1x1)
      (test-equal? "test to transpose correctly with 1xm rows/columns"
          (transpose 1xm)
          t-1xm)
      (test-equal? "test to transpose correctly with nx1 rows/columns"
          (transpose nx1)
          t-nx1)
      (test-equal? "test to transpose correctly with nxm rows/columns"
          (transpose nxm)
          t-nxm)
      (test-equal? "test transpose with non table structure"
          (transpose 'symbol)
          'no-rows-or-columns-given)
      (test-equal? "warn if only row or column is given"
          (transpose '(elem))
          'no-nested-list-given->could-be-row-or-list))
    (test-case "tests for table-by-rows->table-by-columns"
      (test-equal? ""(table->table-by-columns (table-by-rows 0x0)) (table-by-columns 0x0))
      (test-equal? ""(table->table-by-columns (table-by-rows 1x1)) (table-by-columns t-1x1))
      (test-equal? ""(table->table-by-columns (table-by-rows 1xm)) (table-by-columns t-1xm))
      (test-equal? ""(table->table-by-columns (table-by-rows nx1)) (table-by-columns t-nx1))
      (test-equal? ""(table->table-by-columns (table-by-rows nxm)) (table-by-columns t-nxm))
      )
    (test-case "tests for table-by-rows->table-by-rows"
      (test-equal? ""(table->table-by-rows (table-by-rows 0x0)) (table-by-rows 0x0))
      (test-equal? ""(table->table-by-rows (table-by-rows 1x1)) (table-by-rows 1x1))
      (test-equal? ""(table->table-by-rows (table-by-rows 1xm)) (table-by-rows 1xm))
      (test-equal? ""(table->table-by-rows (table-by-rows nx1)) (table-by-rows nx1))
      (test-equal? ""(table->table-by-rows (table-by-rows nxm)) (table-by-rows nxm))
      )
    (test-case "tests for table-by-columns->table-by-rows"
      (test-equal? ""(table->table-by-columns (table-by-columns 0x0)) (table-by-rows 0x0))
      (test-equal? ""(table->table-by-columns (table-by-columns 1x1)) (table-by-rows t-1x1))
      (test-equal? ""(table->table-by-columns (table-by-columns 1xm)) (table-by-rows t-1xm))
      (test-equal? ""(table->table-by-columns (table-by-columns nx1)) (table-by-rows t-nx1))
      (test-equal? ""(table->table-by-columns (table-by-columns nxm)) (table-by-rows t-nxm))
      )
    (test-case "tests for table-by-columns->table-by-columns"
      (test-equal? ""(table->table-by-columns (table-by-columns 0x0)) (table-by-columns 0x0))
      (test-equal? ""(table->table-by-columns (table-by-columns 1x1)) (table-by-columns 1x1))
      (test-equal? ""(table->table-by-columns (table-by-columns 1xm)) (table-by-columns 1xm))
      (test-equal? ""(table->table-by-columns (table-by-columns nx1)) (table-by-columns nx1))
      (test-equal? ""(table->table-by-columns (table-by-columns nxm)) (table-by-columns nxm))
      )
    (test-case "test for table->table-by-columns and table->table-by-rows with unexpected data"
      (test-equal? ""(table->table-by-columns 'other) 'no-table-given)
      (test-equal? ""(table->table-by-rows 'other) 'no-table-given))
    (test-case "test for table struct guards"
      (test-exn "" (table-by-columns 'other) (lambda () #t))
      (test-exn "" (table-by-rows 'other) (lambda () #t)))
  )))


(define (execute-tests) 
  (run-tests transpose-tests)
)