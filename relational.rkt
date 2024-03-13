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
             #:transparent
             #| #:methods gen:custom-write
                [(define (write-proc db out mode)
                   (for ([rel (Db-rels db)])
                     (displayln (car rel) out)
                     (display (Rel-table (cdr rel)) out)
                  (newline out)))] |#)

(struct/lens Rel (table rules)
             #:transparent)

(define (Db-rel-lens rel-name)
  (lens-compose
   (assoc-lens rel-name)
   Db-rels-lens))

(define/f (query-table name db)
  (view-curried (lens-compose
                 Rel-table-lens
                 (Db-rel-lens name))
                db))

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
   '("item" "description" "location")
   '("item")
   '(("space" "the player shouldn't ever encounter this object" "space")
     ("living room" "the room that you live in" "space")
     ("player" "It's meeee!" "living room")
     ("couch" "it's like a long and soft chair" "living room")
     ("basket" "a woven container" "living room")
     ("drum" "you beat it to make rhythms" "basket")
     ("harmonica" "a mouth organ" "basket")
     ("rattle" "shake it, baby" "basket")
     ("dining room" "the room for dining" "space")
     ("table" "a raised surface for putting things on" "living room")
     ("bathroom" "It's not just for bathing!" "space")
     ("toilet" "where the real work gets done" "bathroom"))))

(define containers
  (make-Table
   '("item" "capacity")
   '("item")
   '(("space" 'infinite)
     ("living room" 1000)
     ("basket" 10)
     ("dining room" 1000)
     ("bathroom" 1000))))

(define leads-to
  (make-Table
   '("from" "to")
   '("from" "to")
   '(("living room" "dining room")
     ("living room" "bathroom")
     ("bathroom" "living room"))))

(define db
  ($ (Db
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
                                "things"))))
       (cons "leads-to"
             (Rel leads-to
                  (list
                   (foreign-key '("from") "containers")
                   (foreign-key '("to") "containers"))))))
     (insert-into "things"
                  (make-row ("item" "table")
                            ("description" "a raised surface for putting things on")
                            ("location" "dining room")))
     (insert-into "leads-to"
                  (make-row ("from" "dining room")
                            ("to" "living room")))))