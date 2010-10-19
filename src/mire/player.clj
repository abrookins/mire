(ns mire.player
  (:use [mire.rooms :only [rooms]]))

(def default-prompt "> ")
  
(def *player-streams* (ref {}))

(defrecord Player [name current-room inventory prompt])

(defn carrying?
  [thing player]
  (some #{(keyword thing)} @(:inventory player)))

(defn get-unique-player-name [name]
  (if (@*player-streams* name)
    (do (print "That name is in use; try again: ")
        (flush)
        (recur (read-line)))
    name))

(defn make-player
  "Create a new player."
  []
  (Player. nil
           (ref (rooms :start))
           (ref #{})
           default-prompt))

;; Global player definition

(def *player* (ref {}))
