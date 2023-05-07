#lang racket

(define (make-string-hash string-lst) ;this makes my assignment 2 function look bloated
  (define count-words (map (lambda (x) (cons x (count (curry equal? x) string-lst))) string-lst))
  (define total-words (apply + (map cdr count-words)))
  (define frequencies
    (map (lambda (p) (cons (car p) (- 10(* -1 (log (/ (cdr p) total-words) 10))))) count-words))
  (make-immutable-hash frequencies))

(define (main filename) ;create three hash tables and compare them
  (define (clean-string input-string) ;remove special chars and switch to lower case
    (define (replace-chars string)(regexp-replace* #px"[^[:alpha:][:digit:][:space:]]" string ""))
    (string-split (string-downcase(replace-chars input-string)))) ; returns list of clean substrings
  (define (compare-hash-tables known-author mystery-author) ;compare 2 hash tables
    (define frequency-differences
      (filter number?(hash-map mystery-author
                               (lambda (k v)(when (hash-has-key? known-author k)
                                              (abs (- v (hash-ref known-author k))))))))
    (/ (foldl + 0 frequency-differences) (length frequency-differences)))
  (define (read-file-as-string file) ;open file and port to string, port automatically closes
    (clean-string (port->string (open-input-file file))))
  (define mystery-hash (make-string-hash (read-file-as-string filename))) 
  (if (> (compare-hash-tables (make-string-hash (read-file-as-string "Doyle.txt")) mystery-hash)
         (compare-hash-tables (make-string-hash (read-file-as-string "Lovecraft.txt")) mystery-hash))
      (println "Probably Lovecraft")
      (println "Probably Doyle")))

(main "mystery1.txt")
(main "mystery2.txt")
