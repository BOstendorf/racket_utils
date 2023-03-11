#lang racket
(require  rackunit
          rackunit/text-ui
          utils/listUtilities)

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

;; tested
(define (one-of-extensions path-inp extension-list)
  (define (matcher inp)
    (match inp
      [(list 'no-path-or-path-string-provided _) (raise-argument-error 'no-valid-path-or-path-string
                                                                        "(or (? path?) (? path-string?))"
                                                                        path-inp)]
      [(list  (? (lambda (p) 
                        (not (equal? (file-or-directory-type p)
                                      'file)))) 
              _) 
        (raise-argument-error 'path-is-no-file "(or (? path?) (? path-string?)) pointing to file" path-inp)]
      [(list _ '()) #t]
      [(list path (and exts (list (? string?) ...))) (matcher (list path (map string->bytes/locale exts)))]
      [(list path (and exts (list (? bytes?) ...))) (and (member (path-get-extension path) exts) #t)]
      [(list _ exts) (raise-argument-error  'no-valid-list-of-extensions 
                                            "(list (or (? string?) (? bytes)) ...)"
                                            exts)]
    ))
  (matcher (list (resolve-path path-inp) extension-list))
)

(define (extension-filter extension-list)
  (curryr one-of-extensions extension-list))

(define (find-files-in-dir path [extension-list '()])
  (let ([path (resolve-path path)])
    (filter (extension-filter extension-list)
            (directory-list-full-paths path)))
  )

;; tested
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

(define one-of-extensions-tests
  (test-suite "tests for one-of-extensions"
    #:before
      (lambda ()
        (mkdir "/home/me/racketUnitTestDir")
        (mkdir "/home/me/racketUnitTestDir/sub1")
        (mkdir "/home/me/racketUnitTestDir/sub2")
        (line-writer "/home/me/racketUnitTestDir/test.txt" '("Line"))
        (line-writer "/home/me/racketUnitTestDir/test.mp4" '("Line"))
        (line-writer "/home/me/racketUnitTestDir/test.mp3" '("Line"))
        (line-writer "/home/me/racketUnitTestDir/test.jpg3" '("Line"))
        (line-writer "/home/me/racketUnitTestDir/sub1/test.txt" '("Line"))
        (line-writer "/home/me/racketUnitTestDir/sub1/test.mp4" '("Line"))
        (line-writer "/home/me/racketUnitTestDir/sub1/test.mp3" '("Line"))
        (line-writer "/home/me/racketUnitTestDir/sub1/test.jpg3" '("Line"))
        
        )
    #:after 
      (lambda () (dir-exists?->delete "/home/me/racketUnitTestDir"))
    (test-suite "path extensions are provided as strings"
      (test-true "path ends on .txt .txt one of extensions"
            (one-of-extensions (string->path "/home/me/racketUnitTestDir/test.txt") '(".txt" ".mp3")))
      (test-true "path-string ends on .txt .txt one of extensions"
            (one-of-extensions "/home/me/racketUnitTestDir/test.txt" '(".txt" ".mp3")))
      (test-exn "provided path does not point to file"
            exn?
            (lambda ()(one-of-extensions (string->path "/home/me/racketUnitTestDir") '(".txt" ".mp3")))
            )
      (test-exn "provided path-string does not point to file"
            exn?
            (lambda ()(one-of-extensions "/home/me/racketUnitTestDir" '(".txt" ".mp3")))
            )
      (test-false "path extension does not match listed extensions"
            (one-of-extensions (string->path "/home/me/racketUnitTestDir/test.txt") '(".mp4")))
      (test-false "path-string extension does not match listed extensions"
            (one-of-extensions "/home/me/racketUnitTestDir/test.txt" '(".mp4")))
    )
    
    (test-suite "path extensions are provided as bytes"
      (test-true "path ends on .txt .txt one of extensions"
            (one-of-extensions (string->path "/home/me/racketUnitTestDir/test.txt") '(#".txt" #".mp3")))
      (test-true "path-string ends on .txt .txt one of extensions"
            (one-of-extensions "/home/me/racketUnitTestDir/test.txt" '(#".txt" #".mp3")))
      (test-exn "provided path does not point to file"
            exn?
            (lambda ()(one-of-extensions (string->path "/home/me/racketUnitTestDir") '(#".txt" #".mp3")))
            )
      (test-exn "provided path does not point to file"
            exn?
            (lambda ()(one-of-extensions "/home/me/racketUnitTestDir" '(#".txt" #".mp3")))
            )
      (test-false "path extension does not match listed extensions"
            (one-of-extensions (string->path "/home/me/racketUnitTestDir/test.txt") '(#".mp4")))
      (test-false "path-string extension does not match listed extensions"
            (one-of-extensions "/home/me/racketUnitTestDir/test.txt" '(#".mp4")))      
    )
    (test-suite "incorrect extension arguments"
      (test-exn "extensions do not map to bytes - path provided"
            exn?
            (lambda ()(one-of-extensions (string->path "/home/me/racketUnitTestDir/test.txt") '('txt 'mp3)))
            )
      (test-exn "extensions do not map to bytes - path string provided"
            exn?
            (lambda () (one-of-extensions "/home/me/racketUnitTestDir/test.txt" '('txt 'mp3))))
    )
    (test-true "no extensions set as restrictions - path-string provided"
          (one-of-extensions "/home/me/racketUnitTestDir/test.txt" '()))
    (test-true "no extensions set as restrictions - path provided"
          (one-of-extensions (string->path "/home/me/racketUnitTestDir/test.txt") '()))
  ))

(define find-files-in-dir-tests
  (test-suite "test for getting correct full paths of files with selected extensions in dir"
    #:before
      (lambda ()
        (mkdir "/home/me/racketUnitTestDir")
        (mkdir "/home/me/racketUnitTestDir/sub1")
        (mkdir "/home/me/racketUnitTestDir/sub2")
        (line-writer "/home/me/racketUnitTestDir/test.txt" '("Line"))
        (line-writer "/home/me/racketUnitTestDir/test.mp4" '("Line"))
        (line-writer "/home/me/racketUnitTestDir/test.mp3" '("Line"))
        (line-writer "/home/me/racketUnitTestDir/test.jpg3" '("Line"))
        (line-writer "/home/me/racketUnitTestDir/sub1/test.txt" '("Line"))
        (line-writer "/home/me/racketUnitTestDir/sub1/test.mp4" '("Line"))
        (line-writer "/home/me/racketUnitTestDir/sub1/test.mp3" '("Line"))
        (line-writer "/home/me/racketUnitTestDir/sub1/test.jpg3" '("Line"))
        
        )
    #:after 
      (lambda () (dir-exists?->delete "/home/me/racketUnitTestDir"))
    (test-true "list full paths of files in dir with sub dir - no extension restriction"
          (and (member* (list (string->path "/home/me/racketUnitTestDir/test.txt")
                              (string->path "/home/me/racketUnitTestDir/test.mp4")
                              (string->path "/home/me/racketUnitTestDir/test.mp3")
                              (string->path "/home/me/racketUnitTestDir/test.jpg3"))
                        (find-files-in-dir "/home/me/racketUnitTestDir"))
                (equal? (length (find-files-in-dir "/home/me/racketUnitTestDir")) 4)))
    (test-true "list full paths of files in dir with no sub dir - no extension restriction"
          (and (member* (list (string->path "/home/me/racketUnitTestDir/sub1/test.txt")
                              (string->path "/home/me/racketUnitTestDir/sub1/test.mp4")
                              (string->path "/home/me/racketUnitTestDir/sub1/test.mp3")
                              (string->path "/home/me/racketUnitTestDir/sub1/test.jpg3"))
                        (find-files-in-dir "/home/me/racketUnitTestDir/sub1"))
                (equal? (length (find-files-in-dir "/home/me/racketUnitTestDir/sub1")) 4)))
    (test-equal? "empty list for listing files in empty dir - no extension restrictions"
                (find-files-in-dir "/home/me/racketUnitTestDir/sub2")
                '())
    (test-true "list full paths of files in dir with sub dir - restriction to extensions .txt and jpg3"
          (and (member* (list (string->path "/home/me/racketUnitTestDir/test.txt")
                              (string->path "/home/me/racketUnitTestDir/test.jpg3"))
                        (find-files-in-dir "/home/me/racketUnitTestDir" '(".txt" ".jpg3")))
                (equal? (length (find-files-in-dir "/home/me/racketUnitTestDir" '(".txt" ".jpg3"))) 2)))
    (test-true "list full paths of files in dir with no sub dir - restriction to extensions .txt and jpg3"
          (and (member* (list (string->path "/home/me/racketUnitTestDir/sub1/test.txt")
                              (string->path "/home/me/racketUnitTestDir/sub1/test.jpg3"))
                        (find-files-in-dir "/home/me/racketUnitTestDir/sub1" '(".txt" ".jpg3")))
                (equal? (length (find-files-in-dir "/home/me/racketUnitTestDir/sub1" '(".txt" ".jpg3"))) 2)))
    (test-equal? "empty list for listing files in empty dir - restriction to extensions .txt and jpg3"
                (find-files-in-dir "/home/me/racketUnitTestDir/sub2" '(".txt" ".jpg3"))
                '())
    (test-true "list full paths of files in dir with sub dir - restriction to extensions not present jpg2"
          (and (member* (list )
                        (find-files-in-dir "/home/me/racketUnitTestDir" '(".jpg2")))
                (equal? (length (find-files-in-dir "/home/me/racketUnitTestDir" '(".jpg2"))) 0)))
    (test-true "list full paths of files in dir with no sub dir - restriction to extensions not present jpg2"
          (and (member* (list )
                        (find-files-in-dir "/home/me/racketUnitTestDir/sub1" '(".jpg2")))
                (equal? (length (find-files-in-dir "/home/me/racketUnitTestDir/sub1" '(".jpg2"))) 0)))
                ))

(define directory-list-full-paths-tests
  (test-suite "tests if directory-list-full-paths-tests returns the correct full paths"
    #:before 
      (lambda () 
        (mkdir "/home/me/racketUnitTestDir") 
        (line-writer "/home/me/racketUnitTestDir/testFile" '("Line"))
        (mkdir "/home/me/racketUnitTestDir/.hidden") 
        (line-writer "/home/me/racketUnitTestDir/.hidden/testFile" '("Line"))
        (mkdir "/home/me/racketUnitTestDir/.hidden/emptyDir"))
    #:after 
      (lambda ()
        (dir-exists?->delete "/home/me/racketUnitTestDir"))
    (test-equal? "list expected paths in hidden dir"
          (directory-list-full-paths "/home/me/racketUnitTestDir/.hidden")
          (list (string->path "/home/me/racketUnitTestDir/.hidden/emptyDir")
                (string->path "/home/me/racketUnitTestDir/.hidden/testFile")))
    (test-equal? "list expected paths in dir with hidden sub dir"
          (directory-list-full-paths "/home/me/racketUnitTestDir")
          (list (string->path "/home/me/racketUnitTestDir/.hidden")
                (string->path "/home/me/racketUnitTestDir/testFile")))
    (test-equal? "list of paths empty for empt dir"
          (directory-list-full-paths "/home/me/racketUnitTestDir/.hidden/emptyDir")
          '())))

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
  (run-tests directory-list-full-paths-tests)
  (run-tests one-of-extensions-tests)
  ;(run-tests find-files-in-dir-tests)
)