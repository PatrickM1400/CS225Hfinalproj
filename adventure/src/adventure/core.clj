(ns adventure.core
  (:gen-class))

(def maze {
  1 [1 2 6] 2 [1 2 3 7] 3 [2 3 4 8] 4 [3 4 5 9] 5 [4 5 10]
  6 [1 6 7 11] 7 [2 6 7 8 12] 8 [3 7 8 9 13] 9 [4 8 9 10 14] 10 [5 9 10 15]
  11 [6 11 12 16] 12 [7 11 12 13 17] 13 [8 12 13 14 18] 14 [9 13 14 15 19] 15 [10 14 15 20]
  16 [11 16 17 21] 17 [12 16 17 18 22] 18 [13 17 18 19 23] 19 [14 18 19 20 24] 20 [15 19 20 25]
  21 [16 21 22] 22 [17 21 22 23] 23 [18 22 23 24] 24 [19 23 24 25] 25 [20 24 25]
  })

(def maze-size (count maze))

(defn rand-unique
  "Pick a random number from 0 to `max-1` that is not in the set `exclude`.  Does not check for errors."
  [max exclude]
  (let [pick (rand-int max)]
      (if (exclude pick) (rand-unique max exclude) pick)))

(defn new-game []
  (let [
    blueloc (rand-unique 26 #{0 1 21 25})
    greenloc (rand-unique 26 #{0 1 21 25 blueloc})
    redloc (rand-unique 26 #{0 1 21 25 blueloc greenloc})
    hammer1loc (rand-unique 26 #{0 1 21 25 blueloc greenloc redloc})
    hammer2loc (rand-unique 26 #{0 1 21 25 blueloc greenloc redloc hammer1loc})
    ]
    {
    :player1 1
    :player2 25
    :blueloc blueloc
    :bluekey :vased
    :greenloc greenloc
    :greenkey :vased
    :redloc redloc
    :redkey :vased
    :hammer1loc hammer1loc
    :hammer1 :hidden
    :hammer2loc hammer2loc
    :hammer2 :hidden
    :status1 :trapped
    :status2 :trapped
    :turn 1}))
  
(defn vector-has [v elt]
      (some #{elt} v))

(defn move-player1 [state]
  (let [pick (rand-nth (-> :player1 state maze))] (if (== pick (state :player1)) (move-player1 state) pick))
  )
(defn move-player2 [state]
  (let [pick (rand-nth (-> :player2 state maze))] (if (== pick (state :player2)) (move-player2 state) pick))
  )

(defn valid-room [curroom moveto] ;return the room to move into, return -1 if can't move into room
  (cond
    (= moveto "n") (let [newroom (- curroom 5)] (if (and (<= curroom 5) (>= curroom 1)) -1 newroom))
    (= moveto "s") (let [newroom (+ curroom 5)] (if (and (<= curroom 25) (>= curroom 21)) -1 newroom))
    (= moveto "e") (let [newroom (+ curroom 1)] (if (= (mod curroom 5) 0) -1 newroom))
    (= moveto "w") (let [newroom (- curroom 1)] (if (= (mod curroom 5) 1) -1 newroom))
    (= moveto "d") curroom
    :else -1
    )
)

(defn collide [state]
  (if (= (state :player1) (state :player2))
    (do (println "Oh, fancy seeing you here")
    (if (= (state :turn) 1)
      (cond 
        (= (state :bluekey) :play2) (assoc state :player2 (move-player2 state) :turn 1 :bluekey :hidden :blueloc (rand-unique 26 #{0 21 (state :redloc) (state :greenloc)}))
        (= (state :greenkey) :play2) (assoc state :player2 (move-player2 state) :turn 1 :greenkey :hidden :greenloc (rand-unique 26 #{0 21 (state :redloc) (state :blueloc)}))
        (= (state :redkey) :play2) (assoc state :player2 (move-player2 state) :turn 1 :redkey :hidden :redloc (rand-unique 26 #{0 21 (state :blueloc) (state :greenloc)}))
        :else (assoc state :player2 (move-player2 state) :turn 1)
      )
      (cond 
        (= (state :bluekey) :play1) (assoc state :player1 (move-player1 state) :turn 2 :bluekey :hidden :blueloc (rand-unique 26 #{0 21 (state :redloc) (state :greenloc)}))
        (= (state :greenkey) :play1) (assoc state :player1 (move-player1 state) :turn 2 :greenkey :hidden :greenloc (rand-unique 26 #{0 21 (state :redloc) (state :blueloc)}))
        (= (state :redkey) :play1) (assoc state :player1 (move-player1 state) :turn 2 :redkey :hidden :redloc (rand-unique 26 #{0 21 (state :blueloc) (state :greenloc)}))
        :else (assoc state :player1 (move-player1 state) :turn 2)
      ))
    )
    state)
)

(defn examine-room [state]
  (if (= (state :turn) 1)
    (cond 
      (and (= (state :player1) (state :blueloc)) (= (state :bluekey) :vased)) (println "You find a blue vase, you'll need a hammer to get the key inside")
      (and (= (state :player1) (state :redloc)) (= (state :redkey) :vased)) (println "You find a red vase, you'll need a hammer to get the key inside")
      (and (= (state :player1) (state :greenloc)) (= (state :greenkey) :vased)) (println "You find a green vase, you'll need a hammer to get the key inside")
      (and (= (state :player1) (state :blueloc)) (= (state :bluekey) :hidden)) (println "You find a blue key on the ground")
      (and (= (state :player1) (state :redloc)) (= (state :redkey) :hidden)) (println "You find a red key on the ground")
      (and (= (state :player1) (state :greenloc)) (= (state :greenkey) :hidden)) (println "You find a green key on the ground")
      (and (= (state :player1) (state :hammer1loc)) (= (state :hammer1) :hidden)) (println "You find a hammer on the ground")
      (and (= (state :player1) (state :hammer2loc)) (= (state :hammer2) :hidden)) (println "You find a hammer on the ground")
      :else (println "You find nothing in this room")
      )
    (cond 
      (and (= (state :player2) (state :blueloc)) (= (state :bluekey) :vased)) (println "You find a blue vase, you'll need a hammer to get the key inside")
      (and (= (state :player2) (state :redloc)) (= (state :redkey) :vased)) (println "You find a red vase, you'll need a hammer to get the key inside")
      (and (= (state :player2) (state :greenloc)) (= (state :greenkey) :vased)) (println "You find a green vase, you'll need a hammer to get the key inside")
      (and (= (state :player2) (state :blueloc)) (= (state :bluekey) :hidden)) (println "You find a blue key on the ground")
      (and (= (state :player2) (state :redloc)) (= (state :redkey) :hidden)) (println "You find a red key on the ground")
      (and (= (state :player2) (state :greenloc)) (= (state :greenkey) :hidden)) (println "You find a green key on the ground")
      (and (= (state :player2) (state :hammer1loc)) (= (state :hammer1) :hidden)) (println "You find a hammer on the ground")
      (and (= (state :player2) (state :hammer2loc)) (= (state :hammer2) :hidden)) (println "You find a hammer on the ground")
      :else (println "You find nothing in this room")
      )
  )
)

(defn open-inv [state]
  (println "Here is your inventory")
  (if (= (state :turn) 1)
    (do
      (if (= (state :bluekey) :play1) (print "Blue key, ") (print ""))
      (if (= (state :redkey) :play1) (print "Red key, ") (print ""))
      (if (= (state :greenkey) :play1) (print "Green key, ") (print ""))
      (if (or (= (state :hammer1) :player1) (= (state :hammer2) :player1)) (println "Hammer") (println ""))
    )
    (do
      (if (= (state :bluekey) :play2) (print "Blue key, ") (print ""))
      (if (= (state :redkey) :play2) (print "Red key, ") (print ""))
      (if (= (state :greenkey) :play2) (print "Green key, ") (print ""))
      (if (or (= (state :hammer1) :player2) (= (state :hammer2) :player2)) (println "Hammer") (println ""))
    )
  )
)

(defn drop-obj [state]
    (do (println "Which object would you like to drop?")
    (if (= (state :turn) 1)
      (do 
        (if (= (state :bluekey) :play1) (print "Blue key (blue) ") (print ""))
        (if (= (state :redkey) :play1) (print "Red key (red) ") (print ""))
        (if (= (state :greenkey) :play1) (print "Green key (green) ") (print ""))
        (if (or (= (state :hammer1) :player1) (= (state :hammer2) :player1)) (println "Hammer (hammer) ") (println ""))
        (let [item (read-line)]
          (cond
            (= item "blue") (if (= (state :bluekey) :play1) 
              (do (println "Dropping the blue key")(assoc state :bluekey :hidden :blueloc (state :player1))) 
              (do (println "Invalid input")(conj state)))
            (= item "red") (if (= (state :redkey) :play1) 
                (do (println "Dropping the red key")(assoc state :redkey :hidden :redloc (state :player1))) 
                (do (println "Invalid input")(conj state)))
            (= item "green") (if (= (state :greenkey) :play1) 
                (do (println "Dropping the green key")(assoc state :greenkey :hidden :greenloc (state :player1))) 
                (do (println "Invalid input")(conj state)))
            (= item "hammer") (cond
              (= (state :hammer1) :player1) (do (println "Dropping the hammer") (assoc state :hammer1 :hidden :hammer1loc (state :player1)))
              (= (state :hammer2) :player1) (do (println "Dropping the hammer") (assoc state :hammer2 :hidden :hammer2loc (state :player1)))
              :else (do (print "Invalid input")(conj state))
              )
            :else (do (println "Invalid input")(conj state))
          )))
      (do 
        (if (= (state :bluekey) :play2) (print "Blue key (blue) ") (print ""))
        (if (= (state :redkey) :play2) (print "Red key (red) ") (print ""))
        (if (= (state :greenkey) :play2) (print "Green key (green) ") (print ""))
        (if (or (= (state :hammer1) :player2) (= (state :hammer2) :player2)) (println "Hammer (hammer) ") (println ""))
        (let [item (read-line)]
          (cond
            (= item "blue") (if (= (state :bluekey) :play2) 
              (do (println "Dropping the blue key")(assoc state :bluekey :hidden :blueloc (state :player2))) 
              (do (println "Invalid input")(conj state)))
            (= item "red") (if (= (state :redkey) :play2) 
                (do (println "Dropping the red key")(assoc state :redkey :hidden :redloc (state :player2))) 
                (do (println "Invalid input")(conj state)))
            (= item "green") (if (= (state :greenkey) :play2) 
                (do (println "Dropping the green key")(assoc state :greenkey :hidden :greenloc (state :player2))) 
                (do (println "Invalid input")(conj state)))
            (= item "hammer") (cond
              (= (state :hammer1) :player2) (do (println "Dropping the hammer") (assoc state :hammer1 :hidden :hammer1loc (state :player2)))
              (= (state :hammer2) :player2) (do (println "Dropping the hammer") (assoc state :hammer2 :hidden :hammer2loc (state :player2)))
              :else (do (print "Invalid input")(conj state))
              )
            :else (do (println "Invalid input")(conj state))
          )))
    )
  )
)

(defn take-obj [state]
  (if (= (state :turn) 1) ;player 1
    (cond
      (and (= (state :player1) (state :blueloc)) (= (state :bluekey) :vased))
        (if (or (= (state :hammer1) :player1) (= (state :hammer2) :player1))
         (do (println "You break the blue vase and obtain the blue key!") (assoc state :bluekey :play1))
         (do (println "You can't get to the key, you'll need a hammer to break the vase") (conj state)))
      (and (= (state :player1) (state :redloc)) (= (state :redkey) :vased))
        (if (or (= (state :hammer1) :player1) (= (state :hammer2) :player1))
          (do (println "You break the red vase and obtain the red key!") (assoc state :redkey :play1))
          (do (println "You can't get to the key, you'll need a hammer to break the vase") (conj state)))
      (and (= (state :player1) (state :greenloc)) (= (state :greenkey) :vased))
        (if (or (= (state :hammer1) :player1) (= (state :hammer2) :player1))
          (do (println "You break the green vase and obtain the green key!") (assoc state :greenkey :play1))
          (do (println "You can't get to the key, you'll need a hammer to break the vase") (conj state)))
      (and (= (state :player1) (state :blueloc)) (= (state :bluekey) :hidden))
        (do (println "You pickup the blue key off the ground!") (assoc state :bluekey :play1))
      (and (= (state :player1) (state :redloc)) (= (state :redkey) :hidden))
        (do (println "You pickup the red key off the ground!") (assoc state :redkey :play1))
      (and (= (state :player1) (state :greenloc)) (= (state :greenkey) :hidden))
        (do (println "You pickup the green key off the ground!") (assoc state :greenkey :play1))
      (and (= (state :player1) (state :hammer1loc)) (= (state :hammer1) :hidden))
        (if (= (state :hammer2) :player1)
          (do (println "You already have a hammer") (conj state))
          (do (println "You pickup a hammer") (assoc state :hammer1 :player1)))
      (and (= (state :player1) (state :hammer2loc)) (= (state :hammer2) :hidden))
        (if (= (state :hammer1) :player1)
          (do (println "You already have a hammer") (conj state))
          (do (println "You pickup a hammer") (assoc state :hammer2 :player1)))
      :else (do (println "There is nothing to take in this room") (conj state))
    )
    (cond ;player 2
      (and (= (state :player2) (state :blueloc)) (= (state :bluekey) :vased))
        (if (or (= (state :hammer1) :player2) (= (state :hammer2) :player2))
         (do (println "You break the blue vase and obtain the blue key!") (assoc state :bluekey :play2))
         (do (println "You can't get to the key, you'll need a hammer to break the vase") (conj state)))
      (and (= (state :player2) (state :redloc)) (= (state :redkey) :vased))
        (if (or (= (state :hammer1) :player2) (= (state :hammer2) :player2))
          (do (println "You break the red vase and obtain the red key!") (assoc state :redkey :play2))
          (do (println "You can't get to the key, you'll need a hammer to break the vase") (conj state)))
      (and (= (state :player2) (state :greenloc)) (= (state :greenkey) :vased))
        (if (or (= (state :hammer1) :player2) (= (state :hammer2) :player2))
          (do (println "You break the green vase and obtain the green key!") (assoc state :greenkey :play2))
          (do (println "You can't get to the key, you'll need a hammer to break the vase") (conj state)))
      (and (= (state :player2) (state :blueloc)) (= (state :bluekey) :hidden))
        (do (println "You pickup the blue key off the ground!") (assoc state :bluekey :play2))
      (and (= (state :player2) (state :redloc)) (= (state :redkey) :hidden))
        (do (println "You pickup the red key off the ground!") (assoc state :redkey :play2))
      (and (= (state :player2) (state :greenloc)) (= (state :greenkey) :hidden))
        (do (println "You pickup the green key off the ground!") (assoc state :greenkey :play2))
      (and (= (state :player2) (state :hammer1loc)) (= (state :hammer1) :hidden))
        (if (= (state :hammer2) :player2)
          (do (println "You already have a hammer") (conj state))
          (do (println "You pickup a hammer") (assoc state :hammer1 :player2)))
      (and (= (state :player2) (state :hammer2loc)) (= (state :hammer2) :hidden))
        (if (= (state :hammer1) :player2)
          (do (println "You already have a hammer") (conj state))
          (do (println "You pickup a hammer") (assoc state :hammer2 :player2)))
      :else (do (println "There is nothing to take in this room") (conj state))
    )
  )
)

(defn special-room [state]
  (if (= (state :turn) 1)
      (if   
      (= (state :player1) 21)(do
        (println "Player 1 finds a door")
        (if (and (= (state :bluekey) :play1) (= (state :greenkey) :play1) (= (state :redkey) :play1))
          (do (println "Player 1 unlocks the door and escapes!") (assoc state :status1 :escape))
          (do (println "The door is locked") (assoc state :status1 :trapped :turn 2))))
      (assoc state :turn 2))
    
      (if 
      (= (state :player2) 21)(do
        (println "Player 2 finds a door")
        (if (and (= (state :bluekey) :play2) (= (state :greenkey) :play2) (= (state :redkey) :play2))
          (do (println "Player 2 unlocks the door and escapes!") (assoc state :status2 :escape))
          (do (println "The door is locked") (assoc state :status2 :trapped :turn 1))))
      (assoc state :turn 1))
    )
)

(defn repl [state]
  (println "You find yourself trapped in a series of rooms")
  (println "Find all three keys to unlock the door and escape")
  (println "Beware! There is another player trapped in this maze")
  (println "Be the first player to escape to win")
  (loop [state state]
      (if (and (= (state :status1) :trapped) (= (state :status2) :trapped))
          (do
            (println "")
            (if (= (state :turn) 1) (println "It is player 1's turn") (println "It is player 2's turn"))
            (println "Player 1 is in room" (state :player1) "| Player 2 is in room" (state :player2))
            (println "What would you like to do? Note: Moving will end your turn")
            (println "Move (m), Examine Room (e), Open Inventory (i), Take Object (t), Drop Object (d), Quit (q)")
            (let [action (read-line)] 
              (cond 
                (= action "m")(do 
                  (println "Pick a room to enter (You may stay in your current room)")
                  (println "You can move North (n), South (s), East (e), West (w), Don't move (d)")
                  (let [room (read-line)]
                      (if (= (state :turn) 1)
                        (if (not= (valid-room (state :player1) room) -1)
                          (recur (special-room (collide (assoc state :player1 (valid-room (state :player1) room)))))
                          (cond 
                            (= room "-1") (println "Quitting program") 
                            (= room "0") (do (if (or (= (state :bluekey) :vased) (= (state :bluekey) :hidden))(print "Blue key location" (state :blueloc) " ")(print ""))
                                          (if (or (= (state :greenkey) :vased) (= (state :greenkey) :hidden))(print "Green key location" (state :greenloc) " ")(print ""))
                                          (if (or (= (state :redkey) :vased) (= (state :redkey) :hidden))(print "Red key location" (state :redloc) " ")(print ""))
                                          (if (= (state :hammer1) :hidden)(print "Hammer 1 location" (state :hammer1loc) " ")(print ""))
                                          (if (= (state :hammer2) :hidden)(print "Hammer 2 location" (state :hammer2loc) " ")(print ""))
                                          (recur state))
                            :else (do (println "Invalid Room")(recur state))
                          ))
                        (if (not= (valid-room (state :player2) room) -1)
                          (recur (special-room (collide (assoc state :player2 (valid-room (state :player2) room)))))
                          (cond 
                            (= room "-1") (println "Quitting program") 
                            (= room "0") (do (if (= (state :bluekey) :vased)(print "Blue key location" (state :blueloc) " ")(print ""))
                                          (if (= (state :greenkey) :vased)(print "Green key location" (state :greenloc) " ")(print ""))
                                          (if (= (state :redkey) :vased)(print "Red key location" (state :redloc) " ")(print ""))
                                          (if (= (state :hammer1) :hidden)(print "Hammer 1 location" (state :hammer1loc) " ")(print ""))
                                          (if (= (state :hammer2) :hidden)(print "Hammer 2 location" (state :hammer2loc) " ")(print ""))
                                          (recur state))
                            :else (do (println "Invalid Room")(recur state))
                          )))))
              (= action "e") (do (examine-room state) (recur state))
              (= action "i") (do (open-inv state) (recur state))
              (= action "t") (recur (take-obj state))
              (= action "d") (recur (drop-obj state))
              (= action "q") (println "Quitting program")
              :else (do (println "Invalid Input") (recur state))
                )
              )
            )
          (println "Game Over"))))


(defn -main []
  (repl(new-game)))