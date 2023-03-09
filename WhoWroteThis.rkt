#lang racket

(define (read-file-as-string file) ;open file and port to string, port automatically closes
  (clean-string (port->string (open-input-file file))))

(define (clean-string input-string) ;remove special chars and switch to lower case
  (define (replace-chars string)
    (regexp-replaces string '([#rx"[-‒—“!:;,.?\n\r]" " "]
                              [#rx"\"" " "])))
  (string-split (string-downcase(replace-chars input-string)))) ; returns list of clean substrings

(define (make-string-hash string-lst hash-table)
  (define (get-word-count lst) (count (curry equal? (first lst)) lst)) ;word count for each word
  (hash-set! hash-table (first string-lst) (get-word-count string-lst))

  (if (not (empty? (rest string-lst))) ;recursive call for the rest of the list
      (make-string-hash (filter-not (curry equal? (first string-lst)) string-lst) hash-table)
      (normalized-relative-frequency-hash hash-table)))

(define (normalized-relative-frequency-hash hash-table) ;update hash values with frequencies
  (define word-count (apply + (hash-values hash-table)))
  (hash-map hash-table (lambda (k v) (hash-set! hash-table k (* -1 (log (/ v word-count) 10))))))

(define (compare-hash-tables known-author mystery-author) ;compare 2 hash tables
  (define frequency-differences
    (filter number?(hash-map mystery-author
                             (lambda (k v)(when (hash-has-key? known-author k)
                                     (abs (- v (hash-ref known-author k))))))))
  (/ (foldl + 0 frequency-differences) (length frequency-differences)))
  
(define (main filename) ;create three hash tables and compare them
  (define doyle-hash (make-hash))
  (define lovecraft-hash (make-hash))
  (define mystery-hash (make-hash))
  (make-string-hash (read-file-as-string "Doyle.txt") doyle-hash)
  (make-string-hash (read-file-as-string "Lovecraft.txt") lovecraft-hash)
  (make-string-hash (read-file-as-string filename) mystery-hash)
  (if (> (compare-hash-tables doyle-hash mystery-hash) (compare-hash-tables lovecraft-hash mystery-hash))
      (println "Probably Lovecraft")
      (println "Probably Doyle")))

(main "mystery1.txt")
(main "mystery2.txt")