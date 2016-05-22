#lang racket

; Include extra string functions
(require srfi/1)
(require srfi/13)

; Include data file
(include "Data.rkt")
(include "PointsAndDistance.rkt")

#|
Define new list called actions and put look and quit list inside list called actions.
|#
(define actions `(,@look ,@quit ,@help ,@pick ,@put ,@inventory ,@curr-status))
(define fight-action `(,@fight))
#|
Define decisiontable with list inside list.
Add an extension to list actions.
action list consists of the list look and quit
|#
(define decisiontable `((1 ((north) 2) ,@actions)
                        (2 ((north) 3) ((south) 1) ((east) 5) ,@actions)
                        (3 ((north) 4) ((south) 2) ,@actions)
                        (4 ((south) 3) ,@actions)
                        (5 ((north) 6) ((west) 2) ((south east) 12) ,@actions)
                        (6 ((climb) 7) ((south) 5) ,@actions)
                        (7 ((north west) 9) ,@actions)
                        (8 ((north east) 9) ,@actions)
                        (9 ((south east) 7) ((south west) 8) ((north) 10) ,@actions)
                        (10 ((south) 9) ((north) 11) ((forward) 11) ((crawl) 11) ((go forward) 11) ((move across) 11) ,@actions)
                        (11 ((north) first-game) ,@actions)
                        (99 ,@actions ,@fight-action)
                        (12 ((north west) 5) ,@actions)))

(define objectdb (make-hash)) ; Define objectdb as hashtable
(define inventorydb (make-hash)) ; Define inventorydb as hashtable

; Add object function that takes 3 functions
; db - hashtable it is adding to
; id - of the room
; object - name of object being added to 
(define (add-object db id object)
  (if (hash-has-key? db id) ; Check if hash id (the key) exists
      (let ((record (hash-ref db id))) ; Set record to value of id
        (hash-set! db id (cons object record))) ; Cons another item onto the record
      (hash-set! db id (cons object empty)))) ; Else, if no value is found, add value to empty list

#| Loop through objects list and store room id and key in objectdb |#
(define (add-objects db)
  (for-each ; loop through each item
   (lambda (r)
     ; call add-object on objectdb to add objects
     (add-object db (first r) (second r))) objects))

(add-objects objectdb)

; Function that takes two parameters, hashtable and id
(define (display-objects db id)
  (when (hash-has-key? db id)
    (let* ((record (hash-ref db id)) ; Record is equal to value of id
           (output (string-join record " and ")))
      (when (not (equal? output ""))
        (if (eq? id 'bag) ; If id == bag
            (printf "- You are carrying a ~a.\n" output) ; Print items being carried
            (printf "- You can see a ~a.\n" output)))))) ; Or display item in the room

(define (remove-object-from-room db id str)
  (when (hash-has-key? db id)
    (let* ((record (hash-ref db id)) ; Display value of id from hash table
           (result  (remove (lambda (x) (string-suffix-ci? str x)) record))
           (item (lset-difference equal? record result)))
      (cond ((null? item)
             (printf " - That item is nowhere to be seen..\n"))
            (else
             (printf " - Added ~a to your pouch.\n" (first item))
             (add-object inventorydb 'bag (first item))
             (hash-set! db id result))))))

(define (remove-object-from-inventory db id str)
  (when (hash-has-key? db 'bag)
    (let* ((record (hash-ref db 'bag))
           ; Compare end of strings and remove
           (result (remove (lambda (x) (string-suffix-ci? str x)) record))
           (item (lset-difference equal? record result)))
      (cond ((null? item) ; item not found, then 
             (printf "- You are not carrying that item!\n")) ; display message
            (else
             (printf "- Removed ~a from your pouch.\n" (first item))
             (add-object objectdb id (first item))
             (hash-set! db 'bag result))))))

; Pick item from room, takes 2 parameters, room id and input
(define (pick-item id input)
  ; Split up input, take rest of input and join string again
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-room objectdb id item))) ; Call remove-object-from-room

; Similar as pick-item
; - Put item takes 2 parameters, room id and input
(define (put-item id input)
  ; Split up input, take rest of input and join string again
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-inventory inventorydb id item))) ; Call remove-object-from-inventory

(define (display-inventory)
  (cond ((= (hash-count inventorydb) 0) ; If hash-count is zero
         (printf "- There is nothing in the bag!\n")) ; Then print bag is empty
        ((empty? (hash-ref inventorydb 'bag)) ; If hash-ref inventorydb 'bag is empty
         (printf "- There is nothing in the bag!\n")) ; Then bag is still empty
        (else (display-objects inventorydb 'bag)))) ; Else, bag is full, then call display-objects

#|
Convert symbol to string
(map symbol->string l) - Convert symbols to individual strings in list.
(string-join l) - Join each string inside the list and change list to string
|#
(define (slist->string l)
  (string-join (map symbol->string l)))

(define (get-directions id)
  (let ((record (assq id decisiontable))) ; Get commands and directions from id in decisiontable
    (let* ((result (filter (lambda (n) (number? (second n))) (cdr record)))
           (n (length result)))
      (cond ((= 0 n) ; If n == 0
             (printf "- You are trapped... There is no way to get to Zing's Kingdom.\n"))
            ((= 1 n) ; If n == 1
             (printf "- There is an opening if we go ~a.\n" (slist->string (caar result))))
            (else    ; If not (1 or 0)
             (let* ((losym (map (lambda (x) (car x)) result)) ; Get directions at id and set to losym
                    (lostr (map (lambda (x) (slist->string x)) losym))) ; Convert from list to string and set to lostr
               (printf "- There are multiple openings if we go ~a.\n" (string-join lostr " or "))))))))

#|
Return the rest of the list when record at id is found
(assq id assqlist) --> '(3 "check list")
(cdr '(3 "check list") --> '("check list")
|#
(define (assq-ref assqlist id)
  (cdr (assq id assqlist)))

#|
Return first item of assq-ref function
(assq-ref assqlist id) --> '("check list")
(car (assq-ref assqlist id)) --> "check list"
|#
(define (get-area-description rid)
  (car (assq-ref descriptions rid)))

;(define (lookup room-id direction)
; (car (assq-ref (assq-ref get-directions room-id) direction)))

(define (list-of-lengths keylist tokens)
  (map
   (lambda (x)
     (let ((set (lset-intersection eq? tokens x)))
       ; apply some weighting to the result
       (* (/ (length set) (length x)) (length set)))) keylist ))

; Return the token / room id of direction in decisiontable
(define (lookup id tokens)
  (let* ((record (assq-ref decisiontable id)) ; Display values of id 2
         (keylist (get-keywords id)) ; Get the keywords set in seperate lists
         (index (index-of-largest-number (list-of-lengths keylist tokens))))
    (if index
        (cadr (list-ref record index)) #f)))

(define (index-of-largest-number list-of-numbers)
  (let ((n (car (sort list-of-numbers >))))
    (if (zero? n)
        #f
        (list-index (lambda (x) (eq? x n)) list-of-numbers))))

; Return keywords set in decisiontable
(define (get-keywords id)
  (let ((keys (assq-ref decisiontable id))) ; Show keys and values according to their id
    (map (lambda (key) (car key)) keys))) ; Remove keys from list

(define (startgame initial-id)
  (intro) ; Call introduction from Data.rkt
  (let loop ((id initial-id) 
             (description #t))
    (if description ; If description is true, print area description
        (printf "~a\n" (get-area-description id))
        (printf "")) ; Else, print nothing
    (when description ; When description is true, display objects in room
      (display-objects objectdb id))
    (printf "> ")
    (let* ((input (string-downcase (read-line))) ; String downcase for consistensty and store in input variable
           (string-tokens (string-tokenize input)) ; Change input from string to list of strings and store in string-tokens
           (tokens (map string->symbol string-tokens))) ; Change string-tokens to symbols inside a list and store in tokens
      (let ((response (lookup id tokens)))
        (cond ((number? response) ; If response is number
               (add-distance) ; Add distance
               (add-points) ; Add points
               (loop response #t))
              ((eq? #f response)
               (printf "- I cannot do that actions '~a' right now!\n" input)
               (loop id #f))
              ((eq? response 'look)
               (get-directions id) ; Display directions
               (loop id #f))
              ((eq? response 'pick) ; If reponse is equal to pick
               (pick-item id input) ; Call pick-item function
               (loop id #f))
              ((eq? response 'drop) ; If reponse is equal to drop
               (put-item id input) ; Call put-item function
               (loop id #f))
              ((eq? response 'inventory)
               (display-inventory) ; Display items in inventory
               (loop id #f))
              ((eq? response 'status) ; If reponse is equal to status
               (display-status) ; Call display-status function from PointsAndDistance.rkt
               (loop id #f))
              ((eq? response 'first-game) ; If response is equal to firstgame
               (first-game) ; Go to mini game
               (loop id #f))
              ((eq? response 'help) ; If reponse is equal to help
               (printf helpmsg) ; Display helpmsg function from data.rkt
               (loop id #t)) ; Then display description
              ((eq? response 'quit)
               (printf "- Hmm... Well, you tried...\nThanks for playing.")
               (exit)))))))

(define (first-game)
  (printf "W- Do something! A mythical mammal is running towards you! ")
  (printf "C'mon help!\n")
  (let loop ((running #t) 
             (description #t))
    (if description
        (printf "A vicious Allocamelus is lunging towards you\n")
        (printf ""))
    (printf "> ")
    (let* ((input (string-downcase (read-line))) ; String downcase for consistensty and store in input variable
           (string-tokens (string-tokenize input)) ; Change input from string to list of strings and store in string-tokens
           (tokens (map string->symbol string-tokens))) ; Change string-tokens to symbols inside a list and store in tokens
      (let ((response (lookup 99 tokens)))
        (cond ((eq? #f response)
               (printf "- What in th-!?\n")
               (loop running #f))
              ((eq? response 'inventory)
               (display-inventory)
               (loop running #f))
              ((eq? response 'help)
               (printf "Ah... Snap! Don't you have a- a stick? A sword? .. Something!")
               (loop running #t))
              ((eq? response 'look)
               (loop running #t))
              ((eq? response 'attack)
               
               (define lst (hash-ref inventorydb 'bag))
               (cond ((memq "sword" lst)
                      (printf "You have hit and defeated the Allocamelus with a sword\n")
                      (remv "sword" lst)
                      (win))
                     ((memq "small rock" lst)
                      (printf "You have thrown a rock at the Allocumelus\n")))
               
               (loop running #t))
              ((eq? response 'quit)
               (printf "- Hmm... Well, you tried...\nThanks for playing.")
               (exit)))))))

(startgame 1)