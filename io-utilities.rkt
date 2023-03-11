#lang racket
(require  rackunit
          rackunit/text-ui)

(provide  
  dir-exists?->delete
  mkdir
  resolve-path
  multi-line-writer
  line-writer
  get-name-of-csv-file-path
  get-name-of-md-file-path
  (prefix-out io-utils: execute-tests)
  get-full-path
  one-of-extensions
  extension-filter
  find-files-in-dir
  directory-list-full-paths
  string->path-extension
  increment-file-name
  move-file
  get-move-compatible-filename

  )

(define (increment-file-name file-name) 
  (path-add-extension
    (string->path-element 
      (string-append 
        (path-element->string 
          (path-replace-extension file-name #"")) 
        "1"))
    (if (path-get-extension file-name)
        (path-get-extension file-name)
        #"")))

(define (move-file source-path target-dir-path)
  (if (copy-directory/files 
        source-path   
        (build-path 
              target-dir-path
              (get-move-compatible-filename 
                    (file-name-from-path source-path)
                    (map  file-name-from-path 
                          (find-files-in-dir target-dir-path)))))
      (delete-directory/files source-path)
      (displayln (list "copy error of" source-path))))

(define (get-move-compatible-filename file-name target-file-names)
  (define (on-hash f-name t-names)
    (if (hash-has-key? t-names f-name) (on-hash (increment-file-name f-name) t-names) f-name))
  (define (on-list f-name t-names)
    (if (member f-name t-names) (on-list (increment-file-name f-name) t-names) f-name))
  (match target-file-names
    [(? hash?) (on-hash file-name target-file-names)]
    [(? list?) (on-list file-name target-file-names)]
    [_ 'no-list-or-hash-for-target-name-lookup]))

(define (string->path-extension str)
  (path-get-extension 
      (string->path-element 
          (if (string-prefix? str ".")
              (string-append "prefix" str)
              str))))

(define (get-full-path path-or-path-string)
  (let ([path (resolve-path path-or-path-string)])
    (if (absolute-path? path)
        path
        (build-path (current-directory) path))))

(define (one-of-extensions path-inp extension-list)
  (define (matcher inp)
    (match inp
      [(list 'no-path-or-path-string-provided _) 'no-valid-path-or-path-string]
      [(list  (? (lambda (p) 
                        (not (equal? (file-or-directory-type p)
                                      'file)))) 
              _) 'path-is-no-file]
      [(list _ '()) #t]
      [(list path exts) (member (path-get-extension path) exts)]
    ))
  (matcher (resolve-path path-inp extension-list))
)

(define (extension-filter extension-list)
  (curryr one-of-extensions extension-list))

(define (find-files-in-dir path [extension-list '()])
  (let ([path (resolve-path path)])
    (filter (extension-filter extension-list)
            (directory-list-full-paths path)))
  )

(define (directory-list-full-paths path)
  (let ([path (resolve-path path)])
    (map  (lambda (listed-elem) 
                  (build-path path listed-elem))
          (directory-list path))))

;; tested
(define (dir-exists?->delete dir-path)
  (let ([path (resolve-path dir-path)])
  (if (directory-exists? path) 
      (and (delete-directory/files path) #t)
      #t)))

;; tested
(define (mkdir dir-path)
  (let ([path (resolve-path dir-path)])
    (if (directory-exists? path)
    'directory-exists-already
    (make-directory path)
  )))

;; tested
(define (resolve-path path)
  (match path
    [(and (? string?) 
          (app (lambda  (str) 
                        (string-prefix? str 
                                        "~/")) #t)) (resolve-path (list 'home-rest (substring path 2)))]
    [(list 'home-rest "") (find-system-path 'home-dir)]
    [(list 'home-rest p-string) (build-path (find-system-path 'home-dir) p-string)]
    [(? string?) (string->path path)]
    [(? path-for-some-system?) path]
    [_ 'no-path-or-path-string-provided]))

;; tested manually
(define (multi-line-writer out-file-path list-of-lines)
  (let ([path (resolve-path out-file-path)])
    (for-each (lambda (list-of-strings) 
                      (line-writer path 
                                  list-of-strings))
              list-of-lines)
    #t)
  )

;; tested manually
(define (line-writer out-file-path list-of-strings)
  (let ([path (resolve-path out-file-path)])
    (call-with-output-file* path
                            #:exists 'append
                            (lambda (out-file)
                                    (displayln
                                              (string-join list-of-strings "")
                                              out-file)))))

;;tested
(define (get-name-of-csv-file-path file-path)
  (string-trim (path-element->string (file-name-from-path file-path)) ".csv"))

;;tested
(define (get-name-of-md-file-path file-path)
  (string-trim (path-element->string (file-name-from-path file-path)) ".md"))


;;; ---------------------------------------------
;;; test cases - invoke using thunk execute-tests
;;; ---------------------------------------------

(define resolve-path-tests
  (test-suite "tests for path resolution. Resolve home dir alias path. Get path from path string"
    (test-equal? "resolve alias for home dir"
          (resolve-path "~/")
          (find-system-path 'home-dir))
    (test-case "absolute path from absolute path string"
      (check-true (path? (resolve-path "/home/me")) "expected path /home/me to resolve")
      (check-true (absolute-path? (resolve-path "/home/me")) "expected path /home/me to resolve to absolute path"))
    (test-case "relative path from relative path string"
      (check-true (path? (resolve-path "tmp/path")) "expected path tmp/path to resolve to path")
      (check-true (relative-path? (resolve-path "tmp/path")) "expected path tmp/path to resovle to relative path"))
  ))

(define delete/create-dir-tests
  (test-suite "tests for creation and deletion of directories"
    (check-equal? (mkdir "~/racketUnitTestDir") (void) "try creating dir using string with home-fir alias for location")
    (check-equal? (dir-exists?->delete "~/racketUnitTestDir") #t "try deleting dir with home-dir alias using string for location")))

(define get-name-of-file-path-tests
  (test-suite "test for functions to get file-name from file-path"
  (test-equal? "get .csv filename from path"
        (get-name-of-csv-file-path (string->path "~/home/test/someCSVname.csv"))
        "someCSVname")
  (test-equal? "get .md filename from path"
        (get-name-of-md-file-path (string->path "~/home/test/someMDname.md"))
        "someMDname")
  ))


(define (execute-tests) 
  (run-tests get-name-of-file-path-tests)
  (run-tests delete/create-dir-tests)
  (run-tests resolve-path-tests)
)