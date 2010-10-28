(ns mire.server
  (:use [mire.player]
        [mire.commands :only [discard look execute]]
        [mire.rooms :only [load-rooms]])
  (:use [clojure.contrib.io :only [reader writer]]
        [clojure.contrib.server-socket :only [create-server]]))

(defn- cleanup
  "Drop all inventory and remove player from room and player list."
  [world player]
  (dosync
   (doseq [item @(:inventory @player)]
     (if-not (empty? item)
       (discard item)))
   (commute (:player-streams @world) dissoc (:name @player))
   (commute (:inhabitants @(:current-room @player))
            dissoc (:name @player))))

(defn- mire-handle-client [world in out]
  (binding [*in* (reader in)
            *out* (writer out)]

    (print "\nWhat is your name? ") (flush)
    (let [player (ref (make-player (:rooms @world)))]
      (dosync
       (alter player merge {:name (get-unique-player-name
                      (:player-streams @world) (read-line))})

       (commute (:inhabitants @(:current-room @player)) assoc (:name @player) @player)
       (commute (:player-streams @world) assoc (:name @player) *out*))

      (println (look player world)) (print (:prompt @player)) (flush)

      (try (loop [input (read-line)]
             (when input
               (println (execute player world input))
               (print (:prompt @player)) (flush)
               (recur (read-line))))
           (finally (cleanup world player))))))

(defn -main
  ([port dir]
     (def world (ref {:player-streams (ref {})
                      :items (ref {})
                      :rooms (ref {})}))
     (load-rooms world dir)
     ;; TODO: Make this easier to shut down. 
     (defonce server (create-server
                      (Integer. port)
                      (fn [in out] (mire-handle-client world in out))))
     (println "Launching Mire server on port" port))
  ([port] (-main port "resources/rooms"))
  ([] (-main 3333)))
