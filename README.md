# relational-db
(small) relational databases as Racket data structures

My intended use case for this is as a component of a text-game engine that I'm working on. Doesn't save the database to the disk or anything, this is an in-memory thing only. Also, the database is immutable; even functions like ```insert-into``` just return a new DB without modifying the old one.

**Dependencies:**
- [lens](https://github.com/jackfirth/lens)
- [relation](https://github.com/countvajhula/relation)

## Querying Example
Let's say we have the following tables, which represent the worldstate of a text-based game, the type where the player goes from room to room, picks up objects, and solves puzzles. 

this table, "things", represents the objects in the world. Rooms are also objects.
```
(item        | description                                     | location   )
-----------------------------------------------------------------------------
(space       | the player shouldn't ever encounter this object | space      )
(living room | the room that you live in                       | space      )
(player      | It's meeee!                                     | living room)
(couch       | it's like a long and soft chair                 | living room)
(basket      | a woven container                               | living room)
(drum        | you beat it to make rhythms                     | basket     )
(harmonica   | a mouth organ                                   | basket     )
(rattle      | shake it, baby                                  | basket     )
(dining room | the room for dining                             | space      )
(table       | a raised surface for putting things on          | dining room)
(bathroom    | It's not just for bathing!                      | space      )
(toilet      | where the real work gets done                   | bathroom   )
```

This "leads-to" table represents the connections between rooms. Note that the connections are one-way, and a two-way connection is made of two opposite one-way connections.
```
(from        | to         )
---------------------------
(living room | dining room)
(living room | bathroom   )
(bathroom    | living room)
(dining room | living room)
```

These tables are in the example game-world database in `relational.rkt`.

If we want to know which rooms the player can go to *from* their current *location*, we can run this query:
```racket
($ (query-table "things" game-world)
   (where (col-has "item" "player"))
   (inner-join (match-cols '(("from" "location")))
               (query-table "leads-to" game-world))
   (select '("to")))
```
It returns this table showing that the player can go to the dining room or the bathroom:
```
(to         )
-------------
(dining room)
(bathroom   )
```
If the player types in the "look" command we might show them a list of the items they can see. Here's the query we run for that:
```racket
($ (query-table "things" game-world)
   (where (col-has "item" "player"))
   (inner-join (match-cols '(("location" "location")))
               (query-table "things" game-world))
   (where (negate (col-has "item" "player")))) ;; We don't want to include the player themselves in the list
```
```
(description                     | item   | location   )
--------------------------------------------------------
(it's like a long and soft chair | couch  | living room)
(a woven container               | basket | living room)
```
Notice how we didn't have to ```select``` out the duplicate columns, because ```inner-join``` unions the rows together.

## Querying Functions
The following isn't intended to be complete documentation, just an overview of the most important (or interesting) functions. These functions are all defined in ```Table.rkt```. TODO: Make the query functions process tables lazily (they're all eager currently).

### $
The $ function provides a way to do sequential function application without drowning in indentation and closing parentheses. It also evaluates the functions from left to right, which I find easier to write and read.
```racket
($ x f1 f2 ... fn)
```
evaluates to:
```racket
(fn ... (f2 (f1 x)) ...)
```
This idiom does require the functions to have exactly one argument, so unless otherwise noted all the following functions can be curried automatically (thanks to [this library](https://github.com/countvajhula/relation)).

### where
Basically the filter function for tables
```racket
(where pred table)
```
pred is a predicate function that takes a row and returns a boolean. The row is included if and only if ```pred``` returns true.

### col-has
For use as a predicate argument to ```where```.
```racket
(col-has col val row)
```
Think of this as "Does the column ```col``` of the row have the value ```val```?".

### inner-join
```racket
(inner-join pred table1 table2)
```
Takes the cartesian product of two tables and filters it. Similarly to ```where```, ```pred``` is a predicate function that takes two rows and returns a boolean. The union of the rows is included if and only if ```pred``` returns true.

### match-cols
```racket
(match-cols names row1 row2)
```
For use as a predicate argument to ```inner-join```. The ```names``` agument is a list of the pairs of columns to try and match together. For example, 
```racket 
(match-cols '(("column A" "column 1") ("column B" "column 2")) row-alp row-num)
```
Will try to match column A from ```row-alp``` to column 1 from ```row-num```, and column B from ```row-alp``` to column 2 from ```row-num```. ```match-cols``` will only return true if both pairs match up.
