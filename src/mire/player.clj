(ns mire.player
  (:use [mire.protocols :as protocols])
  (:use [clojure.contrib.string :only [join]]))

(def default-prompt "> ")

; Items are a map from keyword to quantity.
(defrecord Player [name current-room inventory prompt description]
  protocols/Visible
  (short-description
   [player] 
   (str (:name player)))
  (long-description
   [player] 
   (str (:description player))))

(defn carrying?
  [thing player]
  (contains? @(:inventory @player) (keyword thing)))

(defn get-unique-player-name [player-streams name]
  (if (@player-streams name)
    (do (print "That name is in use; try again: ")
        (flush)
        (recur player-streams (read-line)))
    name))

(defn make-player
  "Create a new player."
  [rooms]
  (Player. nil
           (ref (rooms :start))
           (ref {})
           default-prompt
           nil))
