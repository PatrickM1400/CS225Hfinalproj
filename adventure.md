# Adventure Project

Using the `interaction` MP as a starting point, create a text adventure game.

The `state` will have the following key/value pairs:

 - Key: `rooms`, Value: a map where the keys are room handles (e.g., `:foyer`) and the values
   are room descriptions.
 - Key: `items`, Value: a map of items and their descriptions.
 - Key: `adventurer`, Value: a map describing the player

Se below for more details.

## Rooms

A room description will have the following keys:

 - `:desc` -- a long description of the room.  This should be printed the first time an adventurer 
    enters the room, or when the adventurer says `look`.
 - `:title` -- a short description of the room.  Print this on subsequent visits.
 - `:dir` -- a map containing directions and destinations.  E.g. `{:south :wumpus-pen}`.
 - `:contents` -- a set containing the items visible in the room.

Here is an example map:

```
(def init-map
  {:foyer {:desc "The walls are freshly painted but do not have any pictures.  You get the feeling it was just cre
ated
for a game or something."
           :title "in the foyer"
           :dir {:south :grue-pen}
           :contents #{:raw-egg}}
   :grue-pen {:desc "It is very dark.  You are about to be eaten by a grue."
              :title "in the grue pen"
              :dir {:north :foyer}
              :contents #{}}
   })
```

## Items

An item will have two keys:

 - `:desc` -- The long description of the item.  This should be printed when the adventurer looks at or
   examines the item.
 - `:name` -- The name of the item.

Example:

```
(def init-items
 {:raw-egg {:desc "This is a raw egg.  You probably want to cook it before eating it."
            :name "a raw egg" }})
...
```

## Adventurer

The adventurer hashmap should have 6 keys.
  - `:location` -- The key of the current room.
  - `:inventory` -- A set of things the adventurer is holding.
  - `:tick`  -- An integer keeping track of how many moves the adventurer has made.
  - `:seen` -- A set of the rooms the adventurer has already visited.

Here's an example:

```
(def init-adventurer
  {:location :foyer
   :inventory #{}
   :hp 10
   :lives 3
   :tick 0
   :seen #{}})
```

In this example, the code writer has added `:hp` for hit points, and `:lives` for number of lives
left.


In general, feel free to add more fields to the room, adventurer, and item maps if you need them,
and even more fields to the overall state.

# Some example codes that may be useful:

Here is a routine that prints out the status every time the adventurer enters a room.

```
(defn status [state]
  (let [location (get-in [:adventurer :location] state)
        the-map (:map state)]
    (print (str "You are " (-> the-map location :title) ". "))
    (when-not ((get-in [:adventurer :seen] state) location)
      (print (-> the-map location :desc)))
    (update-in state [:adventurer :seen] #(conj % location))))
```

Here is a routine to go somewhere.

```
(defn go [state dir]
  (let [location (get-in [:adventurer :location] player)
        dest ((get-in [:map location :dir] state) dir)]
    (if (nil? dest)
      (do (println "You can't go that way.")
          player)
      (assoc-in state [:adventurer :location] dest))))
```

Here is a `main` function to start the game.

```
(defn -main
  "Initialize the adventure"
  [& args]
  (loop [local-state {:map init-map :adventurer init-adventurer :items init-items}]
    (let [pl (status local-state) 
          _  (println "What do you want to do?")
          command (read-line)]
      (recur local-state (respond pl (canonicalize command))))))
```

# Requirements

Your adventure must satisfy the following requirements:

  - At least 8 rooms
  - At least one object that can be manipulated and transformed into another object
    (E.g., an egg could be cooked, a chest could be unlocked)
  - An action that must be taken with the object to win the game
  - Support at least the following verbs
    - n, north, go north, ... and the four canonical directions.
    - look, examine ... to examine an object or to view the long description of the room
    - i, inventory ... to list the player's inventory
    - take
    - drop
    - quit
  - The `-main` function should start the REPL.
  - It *must* compile and start the game when I type `lein run`.

You will almost certainly want to add other verbs.

For work:

  - You can work in teams of up to three people.  Turn in *one* copy per team.
  - To turn in your work, email me for an appointment so you can demo your game.
  - If you do work in a team, we hope your adventure will be correspondingly larger or more interesting.
  - Grading will be pretty gentle, just meet the minimum requirements to be "checked off"... but keep
    in mind that one day you may want a letter of recommendation, and I will check to see how you did
    on this project. :)
  - It is due the day after final exams end.  Turn it in earlier if you can.
  - To get started, run the command `lein new app adventure`.  The main file will be in `adventure/src/adventure/core.clj`.
