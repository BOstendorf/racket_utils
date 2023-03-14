#lang racket
(require  rackunit
          rackunit/text-ui
          utils/listUtilities
          utils/stringUtilities)

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
  string->path-extension-string
  file?
  directory?
  )

;; tested
(define (increment-file-name file-name)
  (define (matcher inp)
    (match inp
      [(? string?) (matcher (list inp 'ext (string->path-extension-string inp)))]
      [(? path?) (matcher (path->string (file-name-from-path inp)))]
      [(list str 'ext ext) (matcher (list 'name (string-replace str ext "") 'ext ext))]
      [(list 'name str 'ext ext) (string->path (string-append* str "1" (list ext)))]
    ))
  (file-name-from-path (matcher file-name)))

(define (move-file source-path target-dir-path [target-file-names #f])
  (let* (
    [f-name (file-name-from-path source-path)]
    [t-f-names (or  target-file-names 
                    (map  file-name-from-path
                          (find-files-in-dir target-dir-path)))]
    [goal-f-name (get-move-compatible-filename f-name t-f-names)]
    [new-target-names (if (list? t-f-names) 
                          (cons goal-f-name t-f-names) 
                          (hash-set t-f-names goal-f-name 'new))])
    (if (copy-directory/files source-path 
                              (build-path target-dir-path 
                                          goal-f-name))
        (begin  (delete-directory/files source-path)
                new-target-names)
        (begin  (displayln (list "copy error of" source-path))
                t-f-names))))

(define (get-move-compatible-filename file-name target-file-names)
  (define (on-hash f-name t-names)
    (if (hash-has-key? t-names f-name) (on-hash (increment-file-name f-name) t-names) f-name))
  (define (on-list f-name t-names)
    (if (member f-name t-names) (on-list (increment-file-name f-name) t-names) f-name))
  (match target-file-names
    [(? hash?) (on-hash file-name target-file-names)]
    [(? list?) (on-list file-name target-file-names)]
    [_ 'no-list-or-hash-for-target-name-lookup]))


;; tested
(define (string->path-extension str)
  (define (matcher inp)
    (match inp
      [(and str (? string?)(? (curryr string-prefix? "."))) (matcher (list 'dotfile (substring str 1)))]
      [(list 'dotfile str) (string->bytes/locale (string-trim-until str "."))]
      [(and str (? string?)) (string->bytes/locale (string-trim-until str "."))]
      [_ 'something-went-wrong]
    ))
  (matcher (last (string-split str "/")))
  )

;; tested
(define (string->path-extension-string str)
  (bytes->string/locale (string->path-extension str)))

;; tested manually
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

;; tested
(define (file? path)
  (match (resolve-path path)
    [(and (? path?) (app file-or-directory-type 'file)) #t]
    [_ #f]
  ))

;; tested
(define (directory? path)
  (match (resolve-path path)
    [(and (? path?) (app file-or-directory-type 'directory)) #t]
    [_ #f]
  ))

;; tested
(define (extension-filter extension-list)
  (lambda (path) 
          (and  (equal? (file-or-directory-type path) 'file) 
                (one-of-extensions path extension-list))))

;; tested
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
    [(? path-for-some-system?) path]
    [(? path-string?) (string->path path)]
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

(define increment-file-name-tests
  (test-suite "test cases for increment-file-name"
    (test-equal? "path-string incremented"
          (increment-file-name "someDir/some-file.txt")
          (string->path "some-file1.txt"))
    (test-equal? "path increment"
          (increment-file-name (string->path "someDir/some-file.txt.zip"))
          (string->path "some-file1.txt.zip"))
    (test-equal? "only file-name as string"
          (increment-file-name "test.file.name")
          (string->path-element "test1.file.name"))
    ))

(define string->path-extension-tests
  (test-suite "tests for extracting correct extension from path-string"
        (test-equal? "path-string end on .txt"
              (string->path-extension "test.txt")
              #".txt")
        (test-equal? "composite path-string end on .txt"
              (string->path-extension "someDir/test.txt")
              #".txt")
        (test-equal? "path-string end on .jpg.txt"
              (string->path-extension "test.jpg.txt")
              #".jpg.txt")
        (test-equal? "dotfile returns no extension"
              (string->path-extension ".config")
              #"")
        (test-equal? "dotfile in composite path-string returns no extension"
              (string->path-extension "someDir/.config")
              #"")))

(define string->path-extension-string-tests
  (test-suite "tests for extracting correct extension as string"
        (test-equal? "path-string end on .txt"
              (string->path-extension-string "test.txt")
              ".txt")
        (test-equal? "composite path-string end on .txt"
              (string->path-extension-string "someDir/test.txt")
              ".txt")
        (test-equal? "path-string end on .jpg.txt"
              (string->path-extension-string "test.jpg.txt")
              ".jpg.txt")
        (test-equal? "dotfile returns no extension"
              (string->path-extension-string ".config")
              "")
        (test-equal? "dotfile in composite path-string returns no extension"
              (string->path-extension-string "someDir/.config")
              "")))

(define extension-filter-tests
  (let ([dir-path-string "/home/me/racketUnitTestDir"]
        [dir-path (string->path "/home/me/racketUnitTestDir")]
        [f-path-string "/home/me/racketUnitTestDir/test.txt"]
        [f-path (string->path "/home/me/racketUnitTestDir/test.txt")])
  (test-suite "test cases for extension-filter"
    #:before (lambda () (mkdir dir-path)
                        (line-writer f-path '("Line")))
    #:after (lambda () (dir-exists?->delete dir-path))
        (test-true "returns procedure"
              (procedure? (extension-filter '())))
        (test-exn "raises exception when no valid list of extensions provided"
              exn?
              (lambda () ((extension-filter '('txt)) f-path)))
        (test-true "path-string points to file ; empty extension list"
              ((extension-filter '()) f-path-string))
        (test-true "path points to file ; empty extension list"
              ((extension-filter '()) f-path))
        (test-suite "extensions given as strings"
          (test-false "false when path-string points to dir"
                ((extension-filter '(".txt")) dir-path-string))
          (test-false "false when path points to dir"
                ((extension-filter '(".txt")) dir-path))
          (test-false "path-string points to file not in extension list"
                ((extension-filter '(".mp3" ".mp4")) f-path-string))
          (test-false "path points to file not in extension list"
                ((extension-filter '(".mp3" ".mp4")) f-path))
          (test-true "path-string points to file in extension list"
                ((extension-filter '(".mp3" ".txt")) f-path-string))
          (test-true "path points to file in extension list"
                ((extension-filter '(".mp3" ".txt")) f-path))
        )
        (test-suite "extensions given as bytes"
          (test-false "false when path-string points to dir"
                ((extension-filter '(#".txt")) dir-path-string))
          (test-false "false when path points to dir"
                ((extension-filter '(#".txt")) dir-path))
          (test-false "path-string points to file not in extension list"
                ((extension-filter '(#".mp3" #".mp4")) f-path-string))
          (test-false "path points to file not in extension list"
                ((extension-filter '(#".mp3" #".mp4")) f-path))
          (test-true "path-string points to file in extension list"
                ((extension-filter '(#".mp3" #".txt")) f-path-string))
          (test-true "path points to file in extension list"
                ((extension-filter '(#".mp3" #".txt")) f-path))
        )
  )))

(define file?-dir?-predicate-tests
  (test-suite "tests for preicate file? and directory?"
    #:before (lambda () (mkdir "/home/me/racketUnitTestDir")
                        (line-writer "/home/me/racketUnitTestDir/test.txt" '("Line")))
    #:after (lambda () (dir-exists?->delete "/home/me/racketUnitTestDir"))
    (test-true "path-string is file"
          (file? "/home/me/racketUnitTestDir/test.txt"))
    (test-true "path is file"
          (file? (string->path "/home/me/racketUnitTestDir/test.txt")))
    (test-true "path-string is dir"
          (directory? "/home/me/racketUnitTestDir"))
    (test-true "path is dir"
          (directory? (string->path "/home/me/racketUnitTestDir")))
    (test-true "path-string is dir"
          (directory? "/home/me/racketUnitTestDir/"))
    (test-true "path is dir"
          (directory? (string->path "/home/me/racketUnitTestDir/")))
    (test-false "path-string is file"
          (directory? "/home/me/racketUnitTestDir/test.txt"))
    (test-false "path is file"
          (directory? (string->path "/home/me/racketUnitTestDir/test.txt")))
    (test-false "path-string is dir"
          (file? "/home/me/racketUnitTestDir"))
    (test-false "path is dir"
          (file? (string->path "/home/me/racketUnitTestDir")))
    (test-false "path-string is dir"
          (file? "/home/me/racketUnitTestDir/"))
    (test-false "path is dir"
          (file? (string->path "/home/me/racketUnitTestDir/")))
    (test-false "symbol instead of file path"
          (file? 'other))
    (test-false "symbol instead of dir path"
          (directory? 'other))
    ))

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
  (test-suite "tests for correct return in case a path is given instead of a path-string. 
              Being given a path should signal, that the path is already resolved and thus hasn't been to 
              transformed in any way. But since the resolve-path function should serve as a saveguard
              for any used path being used correctly, even if the ~/ shorthand is used, it should be able
              to except paths. That way the user doesn't have to keep that in mind and make sure,
              that he is using a resolved path"
    (test-equal? "given a path, the same path is getting returned"
          (resolve-path (string->path "/home/me/test"))
          (string->path "/home/me/test"))))
  )

(define delete/create-dir-tests
  (test-suite "tests for creation and deletion of directories"
    #:before
      (lambda ()
        (dir-exists?->delete "/home/me/racketUnitTestDir"))
    #:after 
      (lambda ()
        (dir-exists?->delete "/home/me/racketUnitTestDir"))
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
  (run-tests find-files-in-dir-tests)
  (run-tests file?-dir?-predicate-tests)
  (run-tests extension-filter-tests)
  (run-tests string->path-extension-tests)
  (run-tests increment-file-name-tests)
  (run-tests string->path-extension-string-tests)
)