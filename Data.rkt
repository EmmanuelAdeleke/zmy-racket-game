; DATA FILE

; Define new function (no parameters)
(define (intro)
  (printf "||   --   **   Rise of Z.M.Y   **   --   ||\n\n- ")
  
  (printf "Wait, before we start.. Enter your Zing name: \n> ")
  (let ((player (read-line))) ; Get user input and store it in player variable
    ; Introduction message
    (printf "
 [ - - Hello ~a and Welcome to Rise of Z.M.Y! - - ]
 [ - - The dangerous Zing Master Yem is alive and well! - - ]\n
 [ - - Anyway.. we've got to get to Zing's Kingdom before he does otherwise we are all doomed! - - ]
 [ - - This shouldn't be hard at all.. simply guide me through the jungle! And we should make it! - - ]
 * If you need ANYTHING, just write 'help', 'look'.. y'know.. Just try me out a little.\n
   Good luck ~a!\n\n" player player)))

;Help message
(define helpmsg "W-.. well.. what do I tell you? We have to get to the Kingdom and we won't get there at this rate!
There really isn't much more to tell you. But follow the simple logical clues and you'll be alright! 
- North
- North West
- Jump
- Down
- Take
- Grab
- Check
...All that stuff... Gah! you humans...
\nIf you find any bugs you can email 21244017@student.uwl.ac.uk.
Play on!\n\n")

#| 
Define list (descriptions) with three items inside, being a list with two items in each
First item is a symbol, being the room description id, second item is a string, being the room description
|#
(define descriptions '((1 "Hmm... looks like we are by a small cave (by the entrance of the jungle).")
                       (2 "Right... We are by an opening path by a bush.")
                       (3 "We are by the a tree... The only golden tree in the jungle.")
                       (4 "Arg-.. Damn, a large brick wall. We cannot go further.")
                       ; 5 continues from 2
                       (5 "We're standing above foot tracks, this must be the way.")
                       (6 "Damn, a large brick wall. There is a 20 foot ladder infront of you.")
                       (7 "Deeper in the jungle we go..")
                       (8 "You are at a dead-end. A big wall can be seen infront of you..")
                       (9 "We are by an apple tree. (There are large rocks surrounding the tree)")
                       (10 "You are standing infront of a massive bridge. The wiring and structure of the bridge appears to be wind.")
                       (11 "The bridge has collapsed. There's no going back. (The Kingdom can be seen in the distance)")
                       ; From 5, dead end going south east
                       (12 "Just the edge of the cave... other than that... dead end.")))

(define objects '((3 "golden bar")
                  (4 "key")
                  (9 "small rock")
                  (10 "sword")))

#|
In that list is another list with a keyword (descriptions) and a symbol outside of that list
Same as quit

Corresponds to response in main game loop.

If user inputs glance or look around, it evaluates as look.
**Keywords match action
|#
(define look '(((glance) look) 
               ((look around) look) 
               ((look) look) 
               ((examine) look)))

(define quit '(((exit game) quit) 
               ((quit game) quit) 
               ((exit) quit) 
               ((quit) quit)
               ((give up) quit)))

(define help '(((help) help) 
               ((help me) help) 
               ((confused) help) 
               ((stuck) help)))

(define pick '(((get) pick)
               ((pickup) pick)
               ((pick up) pick)
               ((grab) pick)
               ((pick) pick)))

(define put '(((put) drop)
              ((drop) drop)
              ((leave) drop)
              ((place) drop)
              ((remove) drop)))

(define curr-status '(((status) status)
                      ((current status) status)
                      ((progress) status)
                      ((my progress) status)
                      ((points) status)
                      ((distance) status)))

(define inventory '(((inventory) inventory)
                    ((bag) inventory)
                    ((pouch) inventory)
                    ((check) inventory)))

(define fight '(((fight) attack)
                    ((hit) attack)
                    ((stab) attack)
                    ((attack) attack)
                    ((use) attack)))

(define (win)
  (printf "You have won the game. Thank you for playing!\n")
  (printf "Points: ~a\n" (hash-ref status "Points"))
  (printf "Distance travelled: ~a\n" (hash-ref status "Distance Travelled"))
  (exit))
