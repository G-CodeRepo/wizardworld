;;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;;; Name: Katrina Schenk, Gerald Abut Code:Land of Lisp    Date: 4/05/2014
;;;; Course: ICS313        Assignment: 6
;;;; File: schenkk6.lisp

;;;; CHANGE LOG 4/5/2014-4/6/2014
;;;; Gerald Abut
;;;; macros have been modified (mainly the new-path macro)
;;;; the large lines of code that have been commented out is if you want the hard coded option
;;;; currently it is set to dynamically create the word of Hogwarts
;;;; modified walk function to work with dynamic Hogwarts creation (temp patch, will fix soon)
;;;; new function
;;;;     1)  create-hogwarts
;;;;     2)  create-hogwarts-objects
;;;;     3)  create-hogwarts-paths
;;;;     4)  new-paths-easy  (an easy, no validation, version)
;;;;     5)  tel (for teleport), hidden power really
;;;; all you need to do is call (create-hogwarts) and everything should set-up automatically
;;;; when you call (create-hogwarts) you will automatically be teleported there but since the user will be
;;;; using a flying bike, just remove that one piece of code near the very bottom
;;;; let me know if you find any bugs. Enjoy :)

;;;; CHANGE LOG 4/9/2014
;;;; Gerald Abut
;;;; 1) in *commands* under "FASTEN switch the words from "bikeframe and handlebars" 
;;;;    to "handlebars and bikeframe" because the code only works if the input is (fasten handlebars bikeframe)
;;;; 2) changed wording for third floor by removing west door because it does not exist
;;;; 3) changed the wording for second floor by changing "west" door to "east" door 
;;;;    because west door doesn't exist
;;;; 4) in the hogwarts world, once an item is picked up, it should not be seen when player enters (look) 
;;;;    because it should already be on them. pickup function had to be modified which also modifies the
;;;;    *object-location* so that the correct location is displayed and no duplicate items.
;;;;    however, the candy will always show up when user enters (look) in the great-hall, as well as
;;;;    the antidotes. but once you pick up just one candy, you can't pick up another one. I made the player
;;;;    smart enough so that they'll never eat another one. and the antidotes will always be there but will have
;;;;    the same rules as the candy were it can only be picked up once.
;;;; 5) removed "flying-bike" from *objects* because the bike that is built IS the flying bike.
;;;; 6) create an auto-play function to play both the wizard-world and hogwarts automatically
;;;;    (for test or presentation purposes)
;;;; 7) in hitbucket, cast wand, and eat candy functions, I had to remove the commas associated with 's and
;;;;    changed it to "is" instead because using it in a list will create output errors
;;;; 8) great-hall description has been shortened so that the look function can display everything
;;;; 9) hospital-wing description has been shorted so that the look function can display everything

(defparameter *nodes* '((living-room (you are in the living-room.                ;descriptions of locations in game
                            a wizard is snoring loudly on the couch.))
                        (garden (you are in a beautiful garden.
                            there is a well in front of you.))
                        (attic (you are in the attic.
                            there is a giant welding torch in the corner.))
                        (bedroom (you are in the bedroom.
                            there is a small bed in front of you.))))

#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HOGWARTS LOCATIONS
; entrace hall
 (entrance-hall (You are in the Entrance Hall. It is a large cavernous room. lit by torches with ceiling so high it is barely visible. There is a staircase across the hall. There is a doors going west and a smaller doors going south.))

  ; great hall
  (great-hall (You are in the Great Hall. There are four long tables in front of you covered in golden plates and goblets with desserts. Some students are chatting and eating. There is a bowl of candy to your left.))

  ; courtyard
  (courtyard. (You are in the Courtyard. It is a large green grassy square with big trees. There is a book cover in the grass to your right.))

  ; floor 1 hallway
  (floor-1-hallway (You are in the Floor 1 hallway. It is dimly lit with torches and paintings hanging on the walls. There is a door  going east and a door going west. There is a staircase at the end of the hall.))

  ; hospital wing
  (hospital-wing (You are in the hospital wing. Madame Pomfrey is folding bandages near a bed. A sign on a table in front of you says Antidotes for Weasley Wizard Wheezes products. There is a container filled with multicolored antidotes.))

  ; history of magic classrom
  (history-of-magic-classroom (You are in the History of Magic classroom. There is a blackboard and several long tables with chairs. You can see Hagrids cabin from the thick glass windows. There is a wand laying under one of the tables.))

  ; floor 2 hallway
  (floor-2-hallway (You are in the Floor 2 hallway. It is dimly lit with torches and paintings lining the walls. There is an empty corridor going south door going west and a staircase at the end of the hall.))

  ; empty corridor
 (empty-corridor (You are in the empty corridor. There is a ghost floating by the gargoyle statue. It is Nearly Headless Nick! He says if you are going to duel Malfoy you will need a wand and the Standard Book of Spells.))

  ; trophy room
  (trophy-room (You are in the trophy room. Awards trophies and medals fill the room. There are some pages on top of one of the trophy cases. Mrs. Norris is walking towards you. Better get out of here quick.))

  ; floor 3 hallway
  (floor-3-hallway. (You are in the Floor 3 hallway. It is dimly lit with torches and has paintings lining the walls. There are double doors going north and a single door going west. There is a staircase behind you.))

  ; library
  (library. (You are in the Library. There are tens of thousands of books and students quietly studying. There is a house elf fixing bookbindings in the corner.))

  ; dungeons
  (dungeons. (You are in the dungeons. It is darker and colder down here than the rest of the castle. You see Draco Malfoy but he does not see you.))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
|#

(defun describe-location (location nodes)
;;Function takes a location and *nodes* list, returns location description  
  (cadr (assoc location nodes)))

(defparameter *edges* '((living-room (garden west door) 
                                     (bedroom north door)                        ;paths that the players can take to move between locations
                                     (attic upstairs ladder))
                        (garden (living-room east door))
                        (bedroom (living-room south door))
                        (attic (living-room downstairs ladder))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  #|
  ;;; HOGWARTS PATHS
  (entrance-hall (courtyard south door))  ; entrance hall -> courtyard, courtyard
  (courtyard (entrance-hall north door))  ; and back
  (entrance-hall (great-hall west door))  ; entrance hall -> great hall, great hall
  (great-hall (entrance-hall east door))  ; and back
  (entrance-hall (dungeons downstairs stairs))  ;entrance hall -> dungeons, dungeons
  (dungeons (entrance-hall upstairs stairs))    ; and back
  (entrance-hall (floor-1-hallway upstairs stairs)) ;entrance hall -> floor 1 hallway
 
 (floor-1-hallway (entrance-hall downstairs stairs)) ; and back
  (floor-1-hallway (history-of-magic-classroom west door)) ; floor-1-hallway -> history of magic classoom
  (history-of-magic-classroom (floor-1-hallway east door)) ; and back
  (floor-1-hallway (hospital-wing east door)) ; floor-1-hallway -> hospital wing
  (hospital-wing (floor-1-hallway west door))  ; and back
  (floor-1-hallway (floor-2-hallway upstairs stairs)) ; floor 1 hallway -> floor 2 hallway
  
  (floor-2-hallway (floor-1-hallway downstairs stairs)) ; and back
  (floor-2-hallway (empty-corridor south door)) ; floor 2 hallway -> empty corridor
  (empty-corridor (floor-2-hallway north door)) ; and back
  (floor-2-hallway (trophy-room east door)) ;floor 2 halway -> trophy room
  (trophy-room (floor-2-hallway west door)) ; and back
  (floor-2-hallway (floor-3-hallway upstairs stairs)) ; floor 2 hallway -> floor 3 hallway
  
  (floor-3-hallway (floor-2-hallway downstairs stairs)) ;and back
  (floor-3-hallway (library north door)) ; floor-3-hallway -> library
  (library (floor-3-hallway  south door))))
  |#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun describe-path (edge)
;;Function takes an edge, quasiquotes the data to form a description, returns description
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))


(defun describe-paths (location edges)
;;Function takes a location and and *edges* list, finds edges relevent to the location 
;;then converts the edges to descriptions and joins them all 
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defparameter *objects* '(whiskey bucket frog chain book bikeframe handlebars wheel1 wheel2))  ;list of all objects
                         #|candy book-cover antidote wand pages|#
(defparameter *object-locations* '((whiskey living-room)                ;tracks location of each object
                                   (bucket living-room)
                                   (handlebars living-room)
                                   (book bedroom)
                                   (wheel1 bedroom)
                                   (wheel2 attic)
                                   (bikeframe garden)
                                   (chain garden)
                                   (frog garden)))  
                                  ; (flying-bike living-room)))
                                   #|
                                   (candy great-hall)
                                   (book-cover courtyard)
                                   (antidote hospital-wing)
                                   (wand history-of-magic-classrom)
                                   (pages trophy-room))) 
                                   |#

(defparameter *commands* '(("LOOK      prints a description of surroundings")                                   ;help commands
                           ("WALK      move to another location")
                           ("PICKUP    put an object in the current location into your inventory")
                           ("INVENTORY print all of the objects in your possession")
                           ("HAVE      prints the name of the object if it is in your inventory")
                           ("BUILD     use on the bikeframe and wheel1 in the garden")
                           ("ATTACH    use on the bikeframe and wheel2 in the garden")
                           ("FASTEN    use on the handlebars and bikeframe in the garden")
                           ("WELD      use on the chain and bucket in the attic")
                           ("DUNK      use the bucket and the well")
                           ("SPLASH    use the bucket to wake up the wizard")
                           ("FLY       fly to hogwarts on the bike")
                           ("EAT       eat the candy in the great hall")
                           ("TAKE      take the antidote in the hospital-wing")
                           ("BIND      use on the book-cover and pages in the library")
                           ("DUEL      use to duel malfoy in the dungeons")
                           ("THROW     use the bucket to hit malfoy")
                           ("CAST      use expelliarmus to hit malfoy")))

(defun print-help (coms)
;;function prints the *commands* 
  (cond ((null coms) '())
        (t (append(car coms)(print-help (cdr coms))))))

;;following functions call print-help
(defun help()
  (print-help *commands*))                          
  
(defun h()
   (print-help *commands*))
  
(defun ?()
  (print-help *commands*))
  

(defun objects-at (loc objs obj-loc)
;;Function takes a location *objects* list and *object-locations* list, returns list of objects visible from given location
   (labels ((is-at (obj)
              (eq (cadr (assoc obj obj-loc)) loc)))
       (remove-if-not #'is-at objs)))


(defun describe-objects (loc objs obj-loc)
;;Function takes a location *objects* list and *object-locations* list, returns full description of objects visible from given location
   (labels ((describe-obj (obj)
                `(you see a ,obj on the floor.)))
      (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))


(defparameter *location* 'living-room)                            ;var to track plavers current location, initialized to living room


(defun look ()
;;Function calls describe-location describe-paths and describe-objects, returns description of current location and paths/objects visible
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun walk (direction)
;;Function takes in a direction('upstairs 'downstairs 'east 'west),
;;returns: look() of new location if it finds the matching edge. error message if matching edge not found
;;modifed version of walk to have cases for hogwarts
(cond ((or (eq *location* 'entrance-hall) (eq *location* 'great-hall) (eq *location* 'courtyard) (eq *location* 'dungeons) 
           (eq *location* 'floor-1-hallway) (eq *location* 'history-of-magic-classroom) (eq *location* 'hospital-wing)
           (eq *location* 'floor-2-hallway) (eq *location* 'empty-corridor) (eq *location* 'trophy-room)
           (eq *location* 'floor-3-hallway) (eq *location* 'library)) (walk2 direction)) ; go to version 2 of walking
     
      (t   '()
           (labels ((correct-way (edge)
                      (eq (cadr edge) direction)))
             (let ((next (find-if #'correct-way (cdr (assoc *location* *edges*)))))
               (if next 
                   (progn (setf *location* (car next)) 
                     (look))
                 '(you cannot go that way.)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun walk2 (direction)
  ;; this version of walk is only used if you are in hogwarts
  (cond ((eq *location* 'entrance-hall)
         (cond ((eq direction 'south) (setf *location* 'courtyard) (describe-location *location* *nodes*))
               ((eq direction 'west) (setf *location* 'great-hall) (describe-location *location* *nodes*))
               ((eq direction 'downstairs) (setf *location* 'dungeons) (describe-location *location* *nodes*))
               ((eq direction 'upstairs) (setf *location* 'floor-1-hallway) (describe-location *location* *nodes*))
               (t '(you cannot go that way.))))
        ((eq *location* 'courtyard)
         (cond ((eq direction 'north) (setf *location* 'entrance-hall) (describe-location *location* *nodes*))
               (t '(you cannot go that way.))))
        ((eq *location* 'dungeons)
         (cond ((eq direction 'upstairs) (setf *location* 'entrance-hall) (describe-location *location* *nodes*))
               (t '(you cannot go that way.))))
        ((eq *location* 'great-hall)
         (cond ((eq direction 'east) (setf *location* 'entrance-hall) (describe-location *location* *nodes*))
               (t '(you cannot go that way.))))
        ((eq *location* 'floor-1-hallway)
         (cond ((eq direction 'west) (setf *location* 'history-of-magic-classroom) (describe-location *location* *nodes*))
               ((eq direction 'east) (setf *location* 'hospital-wing) (describe-location *location* *nodes*))
               ((eq direction 'downstairs) (setf *location* 'entrance-hall) (describe-location *location* *nodes*))
               ((eq direction 'upstairs) (setf *location* 'floor-2-hallway) (describe-location *location* *nodes*))
               (t '(you cannot go that way.))))
        ((eq *location* 'history-of-magic-classroom)
         (cond ((eq direction 'east) (setf *location* 'floor-1-hallway) (describe-location *location* *nodes*))
               (t '(you cannot go that way.))))
        ((eq *location* 'hospital-wing)
         (cond ((eq direction 'west) (setf *location* 'floor-1-hallway) (describe-location *location* *nodes*))
                (t '(you cannot go that way.))))
        ((eq *location* 'floor-2-hallway)
         (cond ((eq direction 'downstairs) (setf *location* 'floor-1-hallway) (describe-location *location* *nodes*))
               ((eq direction 'south) (setf *location* 'empty-corridor) (describe-location *location* *nodes*))
               ((eq direction 'east ) (setf *location* 'trophy-room) (describe-location *location* *nodes*))
               ((eq direction 'upstairs) (setf *location* 'floor-3-hallway) (describe-location *location* *nodes*))
               (t '(you cannot go that way.))))
        ((eq *location* 'trophy-room)
         (cond ((eq direction 'west) (setf *location* 'floor-2-hallway) (describe-location *location* *nodes*))
               (t '(you cannot go that way.))))
        ((eq *location* 'empty-corridor)
         (cond ((eq direction 'north) (setf *location* 'floor-2-hallway) (describe-location *location* *nodes*))
               (t '(you cannot go that way.))))
        ((eq *location* 'floor-3-hallway)
         (cond ((eq direction 'downstairs) (setf *location* 'floor-2-hallway) (describe-location *location* *nodes*))
               ((eq direction 'north) (setf *location* 'library) (describe-location *location* *nodes*))
               (t '(you cannot go that way.))))
        ((eq *location* 'library)
         (cond ((eq direction 'south) (setf *location* 'floor-3-hallway) (describe-location *location* *nodes*))
               (t '(you cannot go that way.))))
        (t '(you cannot go that way.))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(defun pickup (object)
;;Function takes an object, modifies *object-locations* location to be on body, returns confirmation or error message
  (cond ((member object (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)  
       
         ;;;;;;;;;;;;;;;ADDED CODE FOR FOR HOGWARTS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (setf *object-locations* (remove (list object *location*)  *object-locations*))  ;permanently remove object from the object-locations list
         (cond ((eq *location* 'courtyard) 
                (setf *nodes* (remove (assoc 'courtyard  *nodes*) *nodes*)) ; remove old courtyard data
                ; recreate  courtyard to not include the book
                (new-location 'courtyard 'courtyard. '(It is a large green grassy square with big trees.)))
               ((eq *location* 'history-of-magic-classroom)
                (setf *nodes* (remove (assoc 'history-of-magic-classroom *nodes*) *nodes*))
                ; recreate history-of-magic-classrom without the wand
                (new-location 'history-of-magic-classroom 'history-of-magic-classroom. '(There is a blackboard and several long tables with chairs. You can see Hagrids cabin from the thick glass windows.)))
               ((eq *location* 'trophy-room)
                (setf *nodes* (remove (assoc 'trophy-room *nodes*) *nodes*))     
                ; recreate trophy room without the pages
                (new-location 'trophy-room 'trophy-room. '(Awards trophies and medals fill the room. Mrs. Norris is walking towards you. Better get out of there quick.))))
         ;;;;;;;;;;;;;;;END OF ADDED CODE FOR HOGWARTS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

         `(you are now carrying the ,object))
	  (t '(you cannot get that.))))

(defun inventory ()
;;Function returns items that player is carrying
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

(defun have (object) 
;;Function takes on object, returns object name if player is carrying it and NIL if not 
    (member object (cdr (inventory))))


;  wizards_game part 2

;;Function is custom REPL that works exactly like standard REPL 
(defun game-repl ()
    (let ((cmd (game-read)))
        (unless (eq (car cmd) 'quit)
            (game-print (game-eval cmd))
            (game-repl))))


;;Function allows you to read in from user without them having to use a parenthesis 
;;or quote to call function
(defun game-read ()
    (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
         (flet ((quote-it (x)
                    (list 'quote x)))
           (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(look walk pickup inventory help h ? fly)) ;known commands

;;Function makes sure only certain commands are called
(defun game-eval (sexp)
    (if (member (car sexp) *allowed-commands*)
        (eval sexp)
        '(i do not know that command.)))

;;Looks at each character in the list and modifies it as needed
(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eql item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            (caps (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

;;Function converts symbol list to string
(defun game-print (lst)
    (princ (coerce (tweak-text (coerce (string-trim "() " (prin1-to-string lst)) 'list) t nil) 'string))
  (fresh-line))

(defmacro game-action (command subj obj place &body body)
  `(progn (defun ,command (subject object)
            (if (and (eq *location* ',place)
                     (eq subject ',subj)
                     (eq object ',obj)
                     (have ',subj))
                ,@body
            '(i cant ,command like that.)))
     (pushnew ',command *allowed-commands*)))

;;;;;;;;;;;;;;Wizard house actions;;;;;;;;;;;;;;;;;;;;
;;;;;;;Assemble bike
(defparameter *chain-welded* nil)

(defparameter *wheel1-attached* nil)

(defparameter *wheel2-attached* nil)

(defparameter *bike-built* nil)

(game-action build bikeframe wheel1 garden
             (if (and (have 'wheel1) (have 'bikeframe) (not *wheel2-attached*))
                 (progn (setf *wheel1-attached* 't)
                        '(wheel1 is attached to the bikeframe.))
               '(you do not have the items you ate trying to build)))

(game-action attach bikeframe wheel2 garden
             (if (and (have 'wheel2) (not(null *wheel1-attached*)))
                 (progn (setf *wheel2-attached* 't)
                       '(wheel2 is attached to the bikeframe.))
               '(build bikeframe and wheel1 first.)))

(game-action fasten handlebars bikeframe garden
             (if (and (have 'handlebars) (not(null *wheel2-attached*)))
                 (progn (setf *bike-built* 't)
                       '(the bike is assembled!))
               '(handlebars can be fastened after finishing wheel1 and wheel2.)))


;;;;;;;splash wizard
(game-action weld chain bucket attic
             (if (and (have 'bucket) (not *chain-welded*))
                 (progn (setf *chain-welded* 't)
                        '(the chain is now securely welded to the bucket.))
               '(you do not have a bucket.)))

(defparameter *bucket-filled* nil)
(defparameter *house-game-status* nil)

(game-action dunk bucket well garden
             (if *chain-welded* 
                 (progn (setf *bucket-filled* 't)
                        '(the bucket is now full of water))
               '(the water level is too low to reach.)))


(game-action splash bucket wizard living-room
             (cond ((not *bucket-filled*) '(the bucket has nothing in it.))
                   ((have 'frog) '(the wizard awakens and sees that you stole his frog. 
                                   he is so upset he banishes you to the 
                                   netherworlds- you lose! the end.))
                   (t (progn(setf *house-game-status* 't)
                    '(the wizard awakens from his slumber and greets you warmly. 
                         he hands you the magic low-carb donut and says- fly to Hogwarts 
                         on that bike and retrieve my toad from Draco Malfoy.)))))


;;;;;;;;;;;;;;;;;;;Hogwarts actions;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *at-hogwarts* nil)
(defparameter *binded* nil)
(defparameter *sick* nil)
(defparameter *fighting* nil)

(defun fly ()
            (if *house-game-status*
                 (progn (setf *at-hogwarts* 't)
                       (create-hogwarts))
               '(You need to win the game at the wizards house before going to Hogwarts.)))     

(game-action bind book-cover pages library
             (if (and *at-hogwarts* (have 'book-cover) (have 'pages))
                 (progn (setf *binded* 't)
                   '(You now have The Standard Book of Spells))
             '(You do not have all of the book parts)))
             
             
(game-action duel wand malfoy dungeons
           (if (and *binded* *at-hogwarts* (have 'wand) (null *sick*))
                    (progn (setf *fighting* 't)
                      '(You are now in a duel with Malfoy))
           '(You are not prepared for this duel. You lose. Game over.)))

(game-action hit bucket malfoy dungeons
           (if (not(null *fighting*))
               '(You can not win like that. Use the wand!)
           '(You can only use this command in a duel)))

(game-action cast wand expelliarmus dungeons
           (if (not(null *fighting*))
               '(Malfoy is disarmed! He is running away and he dropped the toad! You win! Game over.)
           '(You can only use this command in a duel)))

(game-action eat candy now great-hall
           (if (have 'candy)
               (progn (setf *sick* 't)
                 '(Oh no. This is a Puking Pastille from Weasleys Wizard Wheezes. Find the antidote before facing malfoy.))
          '(You do not have the candy)))

(game-action take antidote now hospital-wing
           (if (have 'candy)
               (progn (setf *sick* nil)
                 '(You are not sick))
           '(You do not have the antidote)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;HOGWARTS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun create-hogwarts ()

  ;;; CREATE THE MANY LOCATIONS OF HOGWARTS
  ; create entrace hall
  (new-location 'entrance-hall 'entrance-hall. '(It is a large cavernous room lit by torches. There is a staircase across the hall. There is a doors going west and door going south.))

  ; create great hall
  (new-location 'great-hall 'great-hall. '(Four tables are in front of you covered in golden plates goblets and desserts. Students are chatting and eating. A bowl of candy is to your left.))

  ; create courtyard
  (new-location 'courtyard 'courtyard. '(It is a large green grassy square with big trees. There is a book-cover in the grass to your right.))

  ; create floor 1 hallway
  (new-location 'floor-1-hallway 'floor-1-hallway. '(It is dimly lit with torches and paintings hanging on the walls. There is a door going east and a door going west. There is a staircase at the end of the hall.))

  ; create hospital wing
  (new-location 'hospital-wing 'hospital-wing. '(Madame Pomfrey is folding bandages near a bed. A sign on a table says Antidotes for Weasley Wizard Wheezes products. A container is filled with multicolored antidotes.)) 

  ; create history of magic classroom
  (new-location 'history-of-magic-classroom 'history-of-magic-classroom. '(There is a blackboard and several long tables with chairs. You can see Hagrids cabin from the thick glass windows. There is a wand laying under one of the tables.))

  ; create floor 2 hallway
  (new-location 'floor-2-hallway 'floor-2-hallway. '(It is dimly lit with torches and paintings lining the walls. There is an empty corridor going south a door going east and a staircase at the end of the hall.))

  ; create empty corridor
  (new-location 'empty-corridor 'empty-corridor. '(There is a ghost floating by the gargoyle statue. It is Nearly Headless Nick! He says if you are going to duel Malfoy you will need a wand and the Standard Book of Spells.))

  ; create trophy room
  (new-location 'trophy-room 'trophy-room. '(Awards trophies and medals fill the room. There are some pages on top of one of the trophy cases. Mrs. Norris is walking towards you. Better get out of there quick.))

  ; create floor 3 hallway
  (new-location 'floor-3-hallway 'floor-3-hallway. '(It is dimly lit with torches and has paintings lining the walls. There are double doors going north. There is a staircase behind you.))

  ; create library
  (new-location 'library 'library. '(There are tens of thousands of books and students quietly studying. There is a house elf fixing bookbindings in the corner.))

  ;create dungeons
  (new-location 'dungeons 'dungeons. '(It is darker and colder down here than the rest of the castle. You see Draco Malfoy but he does not see you.))

  ;;; HOGWARTS EDGES
  ; create the edges in hogwarts
  (create-hogwarts-paths)
  
  ;;; CREATE HOGWARTS OBJECTS
  (create-hogwarts-objects) 
  ; teleport to hogwarts
   (setf *location* 'entrance-hall)   ; YOU CAN REMOVE THIS IF YOU WANT TO GO TO HOGWARTS MANUALLY

  '(welcome to hogwarts!!!))

(defun create-hogwarts-objects ()
  ;;; create-hogwarts-objects creates the objects necessary to play in hogwarts
  (new-object 'candy 'great-hall)
  (new-object 'book-cover 'courtyard)
  (new-object 'antidote 'hospital-wing)
  (new-object 'wand 'history-of-magic-classroom)
  (new-object 'pages 'trophy-room))

 
(defun create-hogwarts-paths ()
  ;;; CREATE NEW PATHS (VERSION 2: create separate edges without automatically creating an opposite edge)
  (new-path-easy 'entrance-hall 'courtyard 'south 'door)  ; entrance hall -> courtyard, courtyard
  (new-path-easy 'courtyard 'entrance-hall 'north 'door)  ; and back
  (new-path-easy 'entrance-hall 'great-hall 'west 'door)  ; entrance hall -> great hall, great hall
  (new-path-easy 'great-hall 'entrance-hall 'east 'door)  ; and back
  (new-path-easy 'entrance-hall 'dungeons 'downstairs 'stairs)  ;entrance hall -> dungeons, dungeons
  (new-path-easy 'dungeons 'entrance-hall 'upstairs 'stairs)    ; and back
  (new-path-easy 'entrance-hall 'floor-1-hallway 'upstairs 'stairs) ;entrance hall -> floor 1 hallway and back
 
  (new-path-easy 'floor-1-hallway 'entrance-hall 'downstairs 'stairs) ; and back
  (new-path-easy 'floor-1-hallway 'history-of-magic-classroom 'west 'door) ; floor-1-hallway -> history of magic classoom
  (new-path-easy 'history-of-magic-classroom 'floor-1-hallway 'east 'door) ; and back
  (new-path-easy 'floor-1-hallway 'hospital-wing 'east 'door) ; floor-1-hallway -> hospital wing
  (new-path-easy 'hospital-wing 'floor-1-hallway 'west 'door) ; and back
  (new-path-easy 'floor-1-hallway 'floor-2-hallway 'upstairs 'stairs) ; floor 1 hallway -> floor 2 hallway
  
  (new-path-easy 'floor-2-hallway 'floor-1-hallway 'downstairs 'stairs) ; and back
  (new-path-easy 'floor-2-hallway 'empty-corridor 'south 'door) ; floor 2 hallway -> empty corridor
  (new-path-easy 'empty-corridor 'floor-2-hallway 'north 'door) ; and back
  (new-path-easy 'floor-2-hallway 'trophy-room 'east 'door) ; floor 2 halway -> trophy room 
  (new-path-easy 'trophy-room 'floor-2-hallway 'west 'door) ; and back
  (new-path-easy 'floor-2-hallway 'floor-3-hallway 'upstairs 'stairs) ; floor 2 hallway -> floor 3 hallway
 
  (new-path-easy 'floor-3-hallway 'floor-2-hallway 'downstairs 'stairs) ; and back
  (new-path-easy 'floor-3-hallway 'library 'north 'door) ; floor 3 hall way to library
  (new-path-easy 'library 'floor-3-hallway 'south 'door)) ; and back

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;MACROS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro new-object (new-obj place)
  ; adds a new object into the *objects* list and update the *object-locations* list
  `(cond ((eq nil (not (member ,new-obj *objects*))) ; ; check if object already exist
                 (format t "~a already exist~%" ,new-obj) nil) ; object already exist, return nil
         (t       (cond ((eq nil (assoc ,place *nodes*)) ;check if place already exist
                        (format t "~a does not exist~%" ,place) nil) ; place does not exist, return nil    
                       (t (push ,new-obj *objects*) ; update objects list
                          (push (list ,new-obj ,place) *object-locations*) ; update object-location list
                          ,new-obj))))) ;return the new item

(defmacro new-location (new-place new-place-dot describe-new-place)
  ; adds a new location into the *nodes* list
       `(cond ((eq nil (not (assoc ,new-place *nodes*))) ; check if place already exist
                      (format t "~a already exist~%" ,new-place) nil) ; place already exist, return nil
              ; add to *nodes* list the new place along with it's description
              (t (push (list ,new-place (append '(you are in the ,(cadr new-place-dot)) ,describe-new-place)) *nodes*) ;there is nothing here.
                 (list ,new-place ,describe-new-place)))) ; return the new place along with its description

            
(defmacro new-path (place1 place2 direction exit-type symmetric)
  ; adds a new path and updates the *edges* list
  ; a symmetric value not equal to zero indicates that the path will be symmetric
  `(let ((p1 nil) (p2 nil) (dir nil) (exit nil) (sym nil) (new-list nil))
        (setq p1 ,place1) (setq p2 ,place2) (setq dir ,direction) (setq exit ,exit-type) (setq sym ,symmetric)
        (setq new-list '())
  (cond ((assoc p1 *nodes*) ;place1 exist
          (cond ((assoc p2 *nodes*) ;place2 exist
                 (cond ((equal nil (and (assoc p1 *edges*) ; an edge DOES NOT EXIST
                                        (car (assoc p2 (cdr (assoc p1 *edges*)))))) 
                        (push `(,p2  ,dir ,exit) new-list)
                        (push p1 new-list)
                        (push new-list *edges*)
                        ; reset everything
                        (setq p1 ,place1) (setq p2 ,place2) (setq dir ,direction) (setq exit ,exit-type) (setq sym ,symmetric)
                        (setq new-list '())
                        (cond ((zerop ,symmetric) '(created path not symmetric)) ; return path not symmetric
                              ((eq 'north ,direction)
                               (push `(,p1 ,(car (list 'south)) ,exit) new-list)
                               (push p2 new-list)
                               (push new-list *edges*))
                              ((eq 'south ,direction)
                               (push `(,p1 ,(car (list 'north)) ,exit) new-list) 
                               (push p2 new-list)        
                               (push new-list *edges*))
                              ((eq 'east ,direction)
                               (push `(,p1 ,(car (list 'west)) ,exit) new-list)
                               (push p2 new-list)
                               (push new-list *edges*))
                              ((eq 'west ,direction)
                               (push `(,p1 ,(car (list 'east)) ,exit) new-list)
                               (push p2 new-list)
                               (push new-list *edges*))
                              ((eq 'up ,direction)
                               (push `(,p1 ,(car (list 'down)) ,exit) new-list)
                               (push p2 new-list)
                               (push new-list *edges*))
                              ((eq 'down ,direction)
                               (push `(,p1 ,(car (list 'up)) ,exit) new-list)
                               (push p2 new-list)
                               (push new-list *edges*))
                              ((eq 'upstairs ,direction)
                               (push `(,p1 ,(car (list 'downstairs)) ,exit) new-list)
                               (push p2 new-list)
                               (push new-list *edges*))
                              ((eq 'downstairs ,direction)
                               (push `(,p1 ,(car (list 'upstairs)) ,exit) new-list)   
                               (push p2 new-list)    
                               (push new-list *edges*))
                              (t  (push `(,p1 ,(car (list 'opposite-of-where-you-came-from)) ,exit) new-list)
                                  (push p2 new-list)
                                  (push new-list *edges*))))
                            (t 'path-already-exist)))
                     (t 'invalid-second-path)))
              (t 'invalid-first-location))))

(defmacro new-path-easy (pl1 pl2 direc ex-type)
  ;;; non-strick function to create new paths
  `(let ((p_1 nil) (p_2 nil) (dr nil) (ex nil) (nlist nil))
        (setq p_1 ,pl1) (setq p_2 ,pl2) (setq dr ,direc) (setq ex ,ex-type)
        (setq nlist '())
        (push `(,p_2  ,dr ,ex) nlist)
        (push p_1 nlist)
        (push nlist *edges*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;HIDDEN ABILITIES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tel (place)
  ;;; blindly teleport to a location
  (setf *location* place) *location*)

(defun auto-play()
  ;; AUTO PLAY WIZARD GAME AND HOGWARTS GAME
  ;; DOUBLES AS A TESTING UNIT
  ; IN WIZARD WORLD 
  ; living room
  (format t "(look)~%")
  (format t "~a~%~%" (look))
  (format t "(pickup 'whiskey)~%")
  (format t "~a~%~%" (pickup 'whiskey))
  (format t "(pickup 'bucket)~%")
  (format t "~a~%~%"  (pickup 'bucket))
  (format t "(pickup 'handlebars)~%")
  (format t "~a~%~%" (pickup 'handlebars))
  (format t "(walk 'upstairs)~%")
  (format t "~a~%~%" (walk 'upstairs))
  ; attic
  (format t "(pickup 'wheel2)~%")
  (format t "~a~%~%" (pickup 'wheel2))
  (format t "(walk 'downstairs)~%")
  (format t "~a~%~%" (walk 'downstairs))
  ; living room
  (format t "(walk 'north)~%")
  (format t "~a~%~%" (walk 'north))
  ; bed room
  (format t "(pickup 'book)~%")
  (format t "~a~%~%" (pickup 'book))
  (format t "(pickup 'wheel1)~%")
  (format t "~a~%~%" (pickup 'wheel1))
  (format t "(walk 'south)~%")
  (format t "~a~%~%" (walk 'south))
  ; living room
  (format t "(walk 'west)~%")
  (format t "~a~%~%" (walk 'west))
  ; garden
  (format t "(pickup 'bikeframe)~%")
  (format t "~a~%~%" (pickup 'bikeframe))
  (format t "(pickup 'chain)~%")
  (format t "~a~%~%" (pickup 'chain))
  (format t "(build 'bikeframe 'wheel1)~%")
  (format t "~a~%~%" (build 'bikeframe 'wheel1))
  (format t "(attach 'bikeframe 'wheel2)~%")
  (format t "~a~%~%" (attach  'bikeframe 'wheel2))
  (format t "(fasten 'handlebars 'bikeframe)~%")
  (format t "~a~%~%" (fasten 'handlebars 'bikeframe))
  (format t "(walk 'east)~%")
  (format t "~a~%~%" (walk 'east))
  ; living room
  (format t "(walk 'upstairs)~%")
  (format t "~a~%~%" (walk 'upstairs))
  ; attic
  (format t "(weld 'chain 'bucket)~%")
  (format t "~a~%~%" (weld 'chain 'bucket))
  (format t "(walk 'downstairs)~%")
  (format t "~a~%~%" (walk 'downstairs))
  ; living room
  (format t "(walk 'west)~%")
  (format t "~a~%~%" (walk 'west))
  ; garden
  (format t "(dunk 'bucket 'well)~%")
  (format t "~a~%~%" (dunk 'bucket 'well))
  (format t "(walk 'east)~%")
  (format t "~a~%~%" (walk 'east))
  ; living room
  (format t "(splash 'bucket 'wizard)~%")
  (format t "~a~%~%" (splash 'bucket 'wizard))
  (format t "(fly)~%")
  (format t "~a~%~%" (fly))

  ; IN HOGWARTS
  ; entrance hall
  (format t "(walk 'west)~%")
  (format t "~a~%~%" (walk 'west))
  ; great hall
  (format t "(pickup 'candy)~%")
  (format t "~a~%~%" (pickup 'candy))
  (format t "(look)~%")
  (format t "~a~%~%" (look))
  (format t "(eat 'candy 'now)~%")
  (format t "~a~%~%" (eat 'candy 'now))
  (format t "(walk 'east)~%")
  (format t "~a~%~%" (walk 'east))
  ; entrance hall
  (format t "(walk 'upstairs)~%")
  (format t "~a~%~%" (walk 'upstairs))
  ; floor 1 hallway
  (format t "(walk 'east)~%")
  (format t "~a~%~%" (walk 'east))
  ; hospital wing
  (format t "(pickup 'antidote)~%")
  (format t "~a~%~%" (pickup 'antidote))
  (format t "(look)~%")
  (format t "~a~%~%" (look))
  (format t "(take 'antidote 'now)~%")
  (format t "~a~%~%" (take 'antidote 'now))
  (format t "(walk 'west)~%")
  (format t "~a~%~%" (walk 'west))
  ; floor 1 hallway
  (format t "(walk 'downstairs)~%")
  (format t "~a~%~%" (walk 'downstairs))
  (format t "(walk 'south)~%")
  (format t "~a~%~%" (walk 'south))
  ; courtyard
  (format t "(pickup 'book-cover)~%")
  (format t "~a~%~%" (pickup 'book-cover))
  (format t "(look)~%")
  (format t "~a~%~%" (look))
  (format t "walk 'north)~%")
  (format t "~a~%~%" (walk 'north))
  ; entrance hall
  (format t "(walk 'upstairs)~%")
  (format t "~a~%~%" (walk 'upstairs))
  ; floor 1 hallway
  (format t "(walk 'upstairs)~%")
  (format t "~a~%~%" (walk 'upstairs))
  ; floor 2 hallway
  (format t "(walk 'south)~%")
  (format t "~a~%~%" (walk 'south))
  ; empty corridor
  (format t "(walk 'north)~%")
  (format t "~a~%~%" (walk 'north))
  ; floor 2 hallway
  (format t "(walk 'downstairs)~%")
  (format t "~a~%~%" (walk 'downstairs))
  ; floor 1 hallway
  (format t "(walk 'west)~%")
  (format t "~a~%~%" (walk 'west))
  ; history of magic classroom
  (format t "(pickup 'wand)~%")
  (format t "~a~%~%" (pickup 'wand))
  (format t "(look)~%")
  (format t "~a~%~%" (look))
  (format t "(walk 'east)~%")
  (format t "~a~%~%" (walk 'east))
  ; floor 1 hallway
  (format t "(walk 'upstairs)~%")
  (format t "~a~%~%" (walk 'upstairs))
  ; floor 2 hallway
  (format t "(walk 'upstairs)~%")
  (format t "~a~%~%" (walk 'upstairs))
  ; floor 3 hallway
  (format t "(walk 'north)~%")
  (format t "~a~%~%" (walk 'north))
  ; library
  (format t "(bind 'book-cover 'pages)~%")
  (format t "~a~%~%" (bind 'book-cover 'pages))
  (format t "(walk 'south)~%")
  (format t "~a~%~%" (walk 'south))
  ; floor 3 hallway
  (format t "(walk 'downstairs)~%")
  (format t "~a~%~%" (walk 'downstairs))
  ; floor 2 hallway
  (format t "(walk 'east)~%")
  (format t "~a~%~%" (walk 'east))
  ; trophy room
  (format t "(pickup 'pages)~%")
  (format t "~a~%~%" (pickup 'pages))
  (format t "(look)~%")
  (format t "~a~%~%" (look))
  (format t "(walk 'west)~%")
  (format t "~a~%~%" (walk 'west))
  ; floor 2 hallway
  (format t "(walk 'upstairs)~%")
  (format t "~a~%~%" (walk 'upstairs))
  ; floor 3 hallway
  (format t "(walk 'north)~%")
  (format t "~a~%~%" (walk 'north))
  ; library
  (format t "(bind 'book-cover 'pages)~%")
  (format t "~a~%~%" (bind 'book-cover 'pages))
  (format t "(inventory)~%")
  (format t "~a~%~%" (inventory))
  (format t "(walk 'south)~%")
  (format t "~a~%~%" (walk 'south))
  ; floor 3 hallway
  (format t "(walk 'downstairs)~%")
  (format t "~a~%~%" (walk 'downstairs))
  ; floor 2 hallway
  (format t "(walk 'downstairs)~%")
  (format t "~a~%~%" (walk 'downstairs))
  ; floor 1 hallway
  (format t "(walk 'downstairs)~%")
  (format t "~a~%~%" (walk 'downstairs))
  ; entrance hall
  (format t "(walk 'downstairs)~%")
  (format t "~a~%~%" (walk 'downstairs))
  (format t "(dual 'wand 'malfoy)~%")
  (format t "~a~%~%" (duel 'wand 'malfoy))
  (format t "(hit 'bucket 'malfoy)~%")
  (format t "~a~%~%" (hit 'bucket 'malfoy))
  (format t "(cast 'wand 'expelliarmus)~%")
  (format t "~a~%~%" (cast 'wand 'expelliarmus))
  (format t "~a~%~%" '(the frog gives you the power to teleport!!!!))
  (format t "~a~%~%" '(heading back to wizard world to bring back the frog!!!))
  (format t "(tel 'living-room)~%")
  (format t "~a~%~%" (tel 'living-room))
  (format t "(look)~%")
  (format t "~a~%~%" (look))
) 



