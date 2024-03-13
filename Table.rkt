#lang racket

(require
  relation
  lens
  "utilities.rkt"
  "lens-utilities.rkt"
  racket/hash)

(provide
 make-row
 make-Table
 Table-header
 Table-primary-key
 Table-index-lens
 Table-rows
 insert-row
 Table-has-key?
 view-row
 delete-row
 rename-cols
 select
 get-col
 where
 col-has
 inner-join
 match-cols
 natural
 cols-as)

(define/f (get-pos val ls)
  (if (equal? val (car ls))
      0
      (add1 (get-pos val (cdr ls)))))

(define (display-table table out mode)
  (define (pad-col col)
    (define string-col
      (map (curry format "~a") col))
    (define max-length
      (apply max
             (map string-length string-col)))
    (map (λ (str)
           (define length-difference
             (- max-length (string-length str)))
           (format "~a~a"
                   str
                   (make-string length-difference
                                #\space)))
         string-col))
  (define data
    ($ table
       Table-cols
       stream-rest
       (map/f pad-col)
       transpose
       (map/f (intersperse "|"))))
  (define header (stream-first data))
  (newline out)
  (displayln header out)
  (displayln (make-string (string-length (format "~a" header))
                          #\-)
             out)
  (for ([row (stream-rest data)])
    (displayln row out)))

(struct/lens Table (header primary-key index next-i rowhash)
             #:methods gen:custom-write
             [(define write-proc display-table)])

  (define-syntax-rule
    (make-row (col-name val) ...)
    (list (cons col-name val) ...))

  (define/f (make-Table header key row-list)
    (define rowhash
      ($ (map/f cons
              (in-naturals)
              row-list)
       stream->list
       make-immutable-hash))
  (define key-positions
    (map
     (curryr get-pos header)
     key))
  (define index
    ($ row-list
       (app map/f
            (λ (row i)
              (cons
               (map (curry list-ref row)
                    key-positions)
               i))
            _
            (in-naturals))
       stream->list
       make-immutable-hash))
  (Table header key index (length row-list) rowhash))

(define (Table-rows table)
  ($ table
     Table-rowhash
     hash->list
     (map/f
      (compose
       stream->list
       (curry zip
         (cons 'i (Table-header table)))))
     stream->list))

(define Table-rows-lens
  (make-lens
   Table-rows
   (λ (table yx)
     (set-curried
      Table-rowhash-lens
      (make-immutable-hash
       (map (curry map cdr) yx))
      table))))

(define (Table-cols table)
  ($ table
     Table-rowhash
     hash->list
     (curry cons
       (cons 'i (Table-header table)))
     transpose
     ;(curry zip
     ;  (cons 'i (Table-header table)))
     stream->list))

(define Table-cols-lens
  (make-lens
   Table-cols
   (λ (table cols)
     (set-curried
      Table-rowhash-lens
      ($ cols
         (curry map cdr)
         transpose
         stream->list
         make-immutable-hash)
      table))))

(define/f (insert-row row table)
  (define order-row-by
    (compose (curry map cdr)
             (curryr assoc-select row)))
  (define row-by-header
    (order-row-by (Table-header table)))
  (define row-by-prim
    (order-row-by (Table-primary-key table)))
  (define i
    (hash-ref (Table-index table)
              row-by-prim
              (Table-next-i table)))       
  ($ table
     (set-curried
      (lens-compose
       (hash-ref-lens i)
       Table-rowhash-lens)
      row-by-header)
     (set-curried
      (lens-compose
       (hash-ref-lens row-by-prim)
       Table-index-lens)
      i)
     (transform-curried
      Table-next-i-lens
      add1)))

(define (key-ref key table)
  (lens-view
   (lens-compose       
    (hash-ref-lens key)
    Table-index-lens)
   table))

(define/contract/f (Table-has-key? key table)
  (-> list? Table?
      boolean?)
  (hash-has-key? (Table-index table)
                 key))

(define/f (delete-row key table)
  (define (delete-field-key field-lens key)
    (transform-curried
     field-lens
     (curryr hash-remove key))) 
  ($ table
     (delete-field-key
       Table-index-lens
      key)
     (delete-field-key
      Table-rowhash-lens
      (key-ref key table))))

(define/contract/f (view-row key table)
  (-> list? Table?
      (listof pair?))
  ($ table
     Table-rowhash
     (curryr hash-ref
       (key-ref key table))
     (curry map
       cons
       (Table-header table))))


(define (Table-row-lens key)
  (make-lens
   (view-row key)
   (flip insert-row)))

(define/contract/f (alter-row f key table)
  (-> (-> any/c any/c) list? Table?
      Table?)
  ($ table
     (view-row key)
     f
     (curryr insert-row table)))
      
(define/contract/f (rename-cols name-pairs table)
  (-> list? Table? Table?)
  (apply $ table    
    (map
     (match-lambda
       [(list old-name new-name)
        (set-curried
         (lens-compose
          (search-lens old-name)
          Table-header-lens)
         new-name)])
     name-pairs)))

(define/f (select key table)
  ($
   table
   (transform-curried
    Table-cols-lens
    (assoc-select (cons 'i key)))
   (set-curried
    Table-header-lens
    key)
   (if (subset? (list->set
                 (Table-primary-key table))
                (list->set
                 key))
       identity
       (compose
        (set-curried Table-primary-key-lens (list))
        (set-curried Table-index-lens (make-immutable-hash))))))

(define (get-col col-name table)
  ($ table
     (select (list col-name))
     Table-rows
     (curry map
       (compose cdr
                second))))

(define/f (where pred table)
  (define filtered-table
    (transform-curried
     Table-rows-lens
     (curry filter pred)
     table))
  (transform-curried   
   Table-index-lens
   (compose
    make-immutable-hash
    (curry filter
      (compose
       (curry hash-has-key?
         (Table-rowhash
          filtered-table))
       cdr))
    hash->list)
   filtered-table))

(define/f (col-has name value row)
  (= value (cdr (assoc name row))))

(define/contract/f (inner-join pred table1 table2)
  (-> (-> list? list? boolean?) Table? Table?
      Table?)
  (define unindexed-rows
    (compose (curry map cdr)
             Table-rows))
  (define rows
    ($
     (cartesian-product
      (unindexed-rows table1)
      (unindexed-rows table2))
     (curry filter
       (curry apply pred))
     (curry map
       (curry apply
         (list-union #:key car)))))
  (define header
    (list-union (Table-header table1)
                (Table-header table2)))
  (define row-list (map (curry map cdr) rows))
  (make-Table header (list) row-list))

(define/contract/f (match-cols names row1 row2)
  (-> list? list? list? boolean?)
  (define (checked-cdr pair)
    (if pair
        (cdr pair)
        (raise-user-error 'match-cols "column not found")))
  (andmap
   (match-lambda
     [(list name1 name2)
      (= #:key checked-cdr
         (assoc name1 row1)
         (assoc name2 row2))])
   names))   

(define/contract/f (natural row1 row2)
  (-> list? list? boolean?)
  (andmap
   (curry apply =)
   (assoc-intersect (compose (curry map cdr) list)
                    row1
                    row2)))

(define/contract/f (cols-as rename table)
  (-> (-> string? string?) Table? Table?)
  (define new-header
    (map rename (Table-header table)))
  (if (check-duplicates new-header)
      (raise-user-error 'cols-as "duplicate column names are not allowed")
      (set-curried Table-header-lens
                   new-header
                   table)))   

(define things
  (make-Table
   '("item" "description" "hp")
   '("item")
   '(("living room" "the room that you live in" 10)
     ("basket" "a woven container" 5)
     ("table" "a raised surface for putting things on" 7)
     ("drum" "you beat it to make rhythms" 1)
     ("harmonica" "a mouth organ" 2)
     ("rattle" "shake it baby" 2))))

(define contains
  (make-Table
   '("container" "item")
   '("item")
   '(("living room" "basket")
     ("living room" "table")
     ("basket" "drum")
     ("basket" "harmonica")
     ("basket" "rattle"))))