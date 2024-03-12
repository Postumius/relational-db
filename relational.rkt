#lang racket

(require "Table.rkt"
         "lens-utilities.rkt"
         "utilities.rkt"
         relation
         lens)

(provide
 Db
 Db?
 Db-rel-lens
 Rel
 Rel-table-lens
 Constraint
 insert-into
 foreign-key)

(struct/lens Db (rels)
             #:transparent)

(struct/lens Rel (table rules)
             #:transparent)

(define (Db-rel-lens rel-name)
  (lens-compose
   (assoc-lens rel-name)
   Db-rels-lens))

(struct/lens Constraint (name insert delete alter))

(define/f (check-row db row rules accessor)
  (for ([rule rules])
    (or ((accessor rule) db row)
        (raise-user-error 'check-row
                          "~n  operation: ~a~n  on row: ~a~n  violates the rule: ~a~n"
                          ($ accessor
                             object-name
                             symbol->string
                             (curryr string-split "-")
                             cdr
                             car)
                          row
                          (Constraint-name rule)))))

(define/f (insert-into rel-name row db)
  (transform-curried
   (Db-rel-lens rel-name)
   (match-lambda
     [(Rel table rules)
      (check-row db row rules Constraint-insert)
      (Rel (insert-row row table) rules)])
   db))

(define (foreign-key key rel-name)
  (Constraint
   (format "~n    foreign-key from relation: ~a~n" rel-name)
   (λ (db row)
     (hash-has-key?
      (view-curried (lens-compose
                     Table-index-lens
                     Rel-table-lens
                     (Db-rel-lens rel-name))
                    db)
      (map cdr (assoc-select key row))))
   (const #t)
   (const #t)))

(define things
  (make-Table
   '("item" "description" "location" "hp")
   '("item")
   '(("living room" "the room that you live in" #f 10)
     ("basket" "a woven container" "living room" 5)
     ("table" "a raised surface for putting things on" "living room" 7)
     ("drum" "you beat it to make rhythms" "basket" 1)
     ("harmonica" "a mouth organ" "basket" 2)
     ("rattle" "shake it, baby" "basket" 2))))

(define containers
  (make-Table
   '("item" "capacity")
   '("item")
   '(("living room" 1000)
     ("basket" 10))))

(define db
  (Db
   (list
    (cons "things"
          (Rel things
               (list
                (foreign-key '("location")
                             "containers"))))
    (cons "containers"
          (Rel containers
               (list
                (foreign-key '("item")
                             "things")))))))