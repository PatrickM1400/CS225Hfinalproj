(ns adventure.core
  (:gen-class)
  (:require clojure.set))



(def maze {
  1 [1 2 6] 2 [1 2 3 7] 3 [2 3 4 8] 4 [3 4 5 9] 5 [4 5 10]
  6 [1 6 7 11] 7 [2 6 7 8 12] 8 [3 7 8 9 13] 9 [4 8 9 10 14] 10 [5 9 10 15]
  11 [6 11 12 16] 12 [7 11 12 13 17] 13 [8 12 13 14 18] 14 [9 13 14 15 19] 15 [10 14 15 20]
  16 [11 16 17 21] 17 [12 16 17 18 22] 18 [13 17 18 19 23] 19 [14 18 19 20 24] 20 [15 19 20 25]
  21 [16 21 22] 22 [17 21 22 23] 23 [18 22 23 24] 24 [19 23 24 25] 25 [20 24 25]
  })

(def key-names ["blue" "green" "red"])

(defn room-unique
  "Pick a random room from 1 to 25 that is not in the set `exclude`.  Does not check for errors."
  [exclude]
  (let [pick (+ (rand-int 25) 1)]
      (if (exclude pick) (room-unique exclude) pick)))

(defn cur-player [state] (state :turn))
(defn notcur-player [state] (- 1 (state :turn)))

(defn change-turn [state] (assoc state :turn (notcur-player state)))

(defn get-indices [myvec needle]
  (set (map first (filter (fn [pair] (= (second pair) needle)) (map-indexed vector myvec))))
)

(defn new-game []
  (let [
    blueloc (room-unique #{1 21 25})
    greenloc (room-unique #{1 21 25 blueloc})
    redloc (room-unique #{1 21 25 blueloc greenloc})
    hammer1loc (room-unique #{1 21 25 blueloc greenloc redloc})
    hammer2loc (room-unique #{1 21 25 blueloc greenloc redloc hammer1loc})
    ]
    {
    :playerlocs [1 25] ; player 0, called "1" starts at room 1, player 2 starts at room 25
    :playerescaped [false false]
    :keylocs [blueloc greenloc redloc]
    :keystatus [:vased :vased :vased]
    :hammerlocs [hammer1loc hammer2loc]
    :hammerstatus [:hidden :hidden]
    :turn 0}
  )
)

(defn vector-has [v elt]
      (some #{elt} v))

(defn bump-player [playerno state]
  (let [curloc ((state :playerlocs) playerno)]
  (let [pick (rand-nth (get maze curloc))] (if (== pick curloc) (bump-player playerno state) pick))
))

(defn valid-room [curroom moveto] ;return the room to move into, return -1 if can't move into room
  (do
  (println curroom)
  (case moveto
    "n" (let [newroom (- curroom 5)] (if (and (<= curroom 5) (>= curroom 1)) -1 newroom))
    "s" (let [newroom (+ curroom 5)] (if (and (<= curroom 25) (>= curroom 21)) -1 newroom))
    "e" (let [newroom (+ curroom 1)] (if (= (mod curroom 5) 0) -1 newroom))
    "w" (let [newroom (- curroom 1)] (if (= (mod curroom 5) 1) -1 newroom))
    "d" curroom
    -1
  )
  )
)

(defn collide [state]
  (if (apply = (state :playerlocs)) ; both positions equal
    (do 
      (println "Oh, fancy seeing you here")
      ; now, we rehide the other person's first key
      (let [other-player (notcur-player state) first-key (.indexOf (state :keystatus) other-player)]
        (let [bumped-state (assoc-in state [:playerlocs other-player] (bump-player other-player state))]
          (if (not= first-key -1) ; if they have any keys
            (assoc-in (assoc-in bumped-state [:keylocs first-key] (room-unique (conj (set (state :keylocs)) 21))) [:keystatus first-key] :hidden) ; move and hide first key (not to finish room though)
            bumped-state ; else just bump other player
          )
        )
      )
    )
    state
  )
)

(defn examine-room [state]
  (let [curloc ((state :playerlocs) (state :turn)) first-key (.indexOf (state :keylocs) curloc) first-hammer (.indexOf (state :hammerlocs) curloc) has-hammer (not= (.indexOf (state :hammerstatus) (cur-player state)) -1)]
    (cond
      (and (not= first-hammer -1) (= ((state :hammerstatus) first-hammer) :hidden)) (println "You find a hammer on the ground")
      (not= first-key -1) (case ((state :keystatus) first-key)
        :vased (if has-hammer
          (println "You find a" (key-names first-key) "vase with a key inside, you can break it with your hammer!")
          (println "You find a" (key-names first-key) "vase, you'll need a hammer to get the key inside...")
        )
        :hidden (println "You find a" (key-names first-key) "key on the ground")
        (println "You find nothing in this room.")
      )
      :else (println "You find nothing in this room!")
    )
  )
)

(defn open-inv [state]
  (do
    (println "Here is your inventory:")
    (doall (map (fn [keynum] (println (key-names keynum) "key")) (get-indices (state :keystatus) (cur-player state))))
    (if (vector-has (state :hammerstatus) (cur-player state))
      (println "Hammer")
    )
  )
)

(defn drop-obj [state]
  (do 
    (println "Which object would you like to drop?")
    (doall (map (fn [keynum] (println (str (key-names keynum) " key (" (key-names keynum) ")"))) (get-indices (state :keystatus) (cur-player state))))
    (if (vector-has (state :hammerstatus) (cur-player state))
      (println "Hammer (hammer)")
    )
    (let [item (read-line) keyno (.indexOf key-names item) curloc ((state :playerlocs) (cur-player state))]
      (cond
        (not= keyno -1) (if (= ((state :keystatus) keyno) (cur-player state)) ; do we own this key
          (do
            (println "Dropped" (key-names keyno) "key")
            (assoc-in (assoc-in state [:keylocs keyno] curloc) [:keystatus keyno] :hidden)
          )
          (do 
            (println "You don't have that key!")
            state
          )
        )
        (= item "hammer") (let [first-hammer (.indexOf (state :hammerstatus) (cur-player state))]
          (if (not= first-hammer -1)
            (do
              (println "Dropping hammer")
              (assoc-in (assoc-in state [:hammerlocs first-hammer] curloc) [:hammerstatus first-hammer] :hidden)
            )
            (do
              (println "You don't have a hammer!")
              state
            )
          )
        )
        :else (do
          (println "Invalid input.")
          state
        )
      )
    )
  )
)

(defn take-obj [state]
  (let [curplayer (cur-player state) curloc ((state :playerlocs) curplayer) has-hammer (not= (.indexOf (state :hammerstatus) curplayer) -1)]
    ; Is key hidden in room, and if so which one?
    (let [
      keyshere (get-indices (state :keylocs) curloc)
      keyhid (first (clojure.set/intersection keyshere (get-indices (state :keystatus) :hidden)))
      keyvase (first (clojure.set/intersection keyshere (get-indices (state :keystatus) :vased)))
      hammerhere (first (clojure.set/intersection (get-indices (state :hammerlocs) curloc) (get-indices (state :hammerstatus) :hidden)))
      ]
      (cond 
        (not= keyhid nil) (do
          (println "You pick up the" (key-names keyhid) "key off the ground!")
          (assoc-in state [:keystatus keyhid] (cur-player state))
        )
        (and (not= keyvase nil) has-hammer) (do
          (println "You break the" (key-names keyvase) "vase and obtain the" (key-names keyvase) "key!")
          (assoc-in state [:keystatus keyvase] (cur-player state))
        )
        (and (not= keyvase nil) (not has-hammer)) (do
          (println "You can't get to the key, you'll need a hammer to break the vase")
          state
        )
        (and (not= hammerhere nil) has-hammer) (do
          (println "You already have a hammer.")
          state
        )
        (and (not= hammerhere nil) (not has-hammer)) (do
          (println "You pick up a hammer.")
          (assoc-in state [:hammerstatus hammerhere] (cur-player state))
        )
        :else (do
          (println "There's nothing to take in this room!")
          state
        )
      )
    )
  )
)

(defn special-room [state]
  (let [curplayer (cur-player state)]
    (if (= ((state :playerlocs) curplayer) 21)
      (do
        (println "Player" (+ curplayer 1) "finds a door!")
        (if (and (apply = (state :keystatus)) (= (first (state :keystatus)) curplayer))
          (do
            (println "Player" (+ curplayer 1) "unlocks the door and escapes!")
            (assoc-in state [:playerescaped curplayer] true)
          )
          (do
            (println "The door is locked with three keyholes...")
            state
          )
        )
      )
      state ; nothing fancy if not in special room
    )
  )
)

; TODO: figure out turn changing (happens when moving)
; which actions return an updated state?
(defn repl [state]
  (do
    (println "You find yourself trapped in a series of rooms")
    (println "Find all three keys to unlock the door and escape")
    (println "Beware! There is another player trapped in this maze")
    (println "Be the first player to escape to win")
    (loop [state state]
      (if (every? false? (state :playerescaped)) ; if nobody escaped
        (let [curplayer (cur-player state) curplayerstr (+ curplayer 1)]
          (do
            (println)
            (println (str "It is player " curplayerstr "'s turn"))
            (doall (map (fn [playerno]
              (println (str "Player " (+ playerno 1) " is in room " ((state :playerlocs) playerno)))
            ) (range (count (state :playerescaped)))))
            (println "What would you like to do? Note: Moving will end your turn")
            (println "Move (m), Examine Room (e), Open Inventory (i), Take Object (t), Drop Object (d), Quit (q)")
            (let [action (read-line)] 
              (case action
                "m" (do 
                  (println "Pick a room to enter (You may stay in your current room)")
                  (println "You can move North (n), South (s), East (e), West (w), Don't move (d)")
                  (let [room (read-line) curloc ((state :playerlocs) curplayer)]
                    (let [newroom (valid-room curloc room)]
                      (if (= newroom -1)
                        (do
                          (println "Invalid move direction")
                          (recur state)
                        )
                        (recur (change-turn (special-room (collide (assoc-in state [:playerlocs curplayer] newroom)))))
                      )
                    )
                  )
                )
                "e" (do (examine-room state) (recur state))
                "i" (do (open-inv state) (recur state))
                "t" (recur (take-obj state))
                "d" (recur (drop-obj state))
                "q" (println "Quitting program")
                "0" (do
                  (doall (map (fn [keyno] (println "Key" keyno "at " ((state :keylocs) keyno))) (range (count (state :keystatus)))))
                  (doall (map (fn [hammerno] (println "Hammer" hammerno "at " ((state :hammerlocs) hammerno))) (range (count (state :hammerstatus)))))
                  (recur state)
                )
                (do (println "Invalid Input") (recur state))
              )
            )
          )
    )
    (println "Game Over")))
  )
)


(defn -main []
  (repl(new-game)))