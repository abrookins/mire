(ns mire.server
  (:use [mire.player]
        [mire.commands :only [discard look execute]]
        [mire.rooms :only [add-rooms rooms]])
  (:use [clojure.contrib.io :only [reader writer]]
        [clojure.contrib.server-socket :only [create-server]]))

(defn- cleanup
  "Drop all inventory and remove player from room and player list."
  [player]
  (dosync
   (doseq [item @(:inventory @player)]
     (if-not (empty? item)
       (discard item)))
   (commute *player-streams* dissoc (:name @player))
   (commute (:inhabitants @(:current-room @player))
            dissoc (:name @player))))

(defn- mire-handle-client [in out]
  (binding [*in* (reader in)
            *out* (writer out)]

    ;; We have to nest this in another binding call instead of using
    ;; the one above so *in* and *out* will be bound to the socket
    (print "\nWhat is your name? ") (flush)
    (let [player (ref (make-player))]
      (dosync
       (alter player merge {:name (get-unique-player-name (read-line))})
       (commute (:inhabitants @(:current-room @player)) assoc (:name @player) @player)
       (commute *player-streams* assoc (:name @player) *out*))

      (println (look player)) (print (:prompt @player)) (flush)

      (try (loop [input (read-line)]
             (when input
               (println (execute player input))
               (print (:prompt @player)) (flush)
               (recur (read-line))))
           (finally (cleanup player))))))

(defn -main
  ([port dir]
     (add-rooms dir)
     ;; TODO: Make this easier to shut down. 
     (defonce server (create-server (Integer. port) mire-handle-client))
     (println "Launching Mire server on port" port))
  ([port] (-main port "resources/rooms"))
  ([] (-main 3333)))
