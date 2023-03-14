#lang racket
(require "./listUtilities.rkt")
(require "./io-utilities.rkt")

;; tested
(struct file-image (path name pure-name lines content-type links-to linked-from))

;; tested
(define (add-incomming-link f-image link)
  (struct-copy file-image f-image [linked-from (cons-non-member link (file-image-linked-from f-image))]))

;; tested
(define (add-outgoing-link f-image link)
  (struct-copy file-image f-image [links-to (cons-non-member link (file-image-links-to f-image))]))

;; tested
(define (build-file-image file-path [testing_line_content #f])
  (file-image 
              file-path
              (path-element->string (file-name-from-path file-path))
              (path-element->string 
                            (path-replace-extension 
                                          (file-name-from-path file-path) 
                                          #"")
              )
              (if testing_line_content
                  testing_line_content
                  (file->lines file-path))
              'unknown
              '()
              '()
  )
)

;; tested
(define (file-image-attribute? attr)
  (member attr (list  file-image-path 
                      file-image-name 
                      file-image-pure-name 
                      file-image-lines 
                      file-image-content-type 
                      file-image-links-to 
                      file-image-linked-from)))

;; tested
(define (f-image->f-image-by-pure-name f-image)
  (cons (file-image-pure-name f-image) f-image))

;; tested
(define (f-images->f-images-by-pure-name-hash f-images)
  (make-immutable-hash (map (lambda (f-image) 
                                    (f-image->f-image-by-pure-name f-image))
                            f-images)))

;; tested
(define (hash-update-content-type-of f-image-key f-image-hash c-type)
  (let ([f-image (hash-ref f-image-hash f-image-key #f)])
    (or (and  f-image
          (hash-set f-image-hash 
                    f-image-key 
                    (struct-copy  file-image 
                                  f-image 
                                  [content-type c-type])))
        f-image-hash)))



(define (ls-dir->f-image-by-attr-hash dir-path f-image-attr)
  (if (not (file-image-attribute? f-image-attr))
      (raise-argument-error 'ls-dir->fimage-by-attr-hash "file-image-attribute? -> one of 
                              file-image-path 
                              file-image-name 
                              file-image-pure-name 
                              file-image-lines 
                              file-image-content-type 
                              file-image-links-to 
                              file-image-linked-from"
                            1 dir-path)
      (make-immutable-hash 
                      (map (lambda  (f-image)
                                    (cons (f-image-attr f-image)
                                          f-image))
                            (map build-file-image (find-files-in-dir dir-path)))))
)

(provide (all-defined-out))