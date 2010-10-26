(ns mire.commands
  (:use [mire.rooms :only [rooms room-contains?]]
        [mire.player :only [carrying? *player-streams*]]
        [mire.protocols :as protocols]
        [mire.util :as util]
        [mire.items :only [*items*]])
  (:use [clojure.contrib.string :only [join]]))

(defn -short-description
  "Call short-description on each item in a collection.  Call
  lookup-fn to find the target object given a key."
  [lookup-fn col]
  (join "\n" (map #(short-description (lookup-fn %)) col)))

;; Command functions

(defn look "Get a description of the surrounding environs and its contents."
  ;; TODO: look [object]
  [player]
  (str (:desc @(:current-room @player))
       "\nExits: " (keys @(:exits @(:current-room @player))) "\n"
       (-short-description
        #(@*items* %) (keys @(:items @(:current-room @player))))
       (-short-description ;; TODO: Players should follow items pattern.
        #(identity %) (vals @(:inhabitants @(:current-room @player))))))

(defn move
  "\"♬ We gotta get out of this place... ♪\" Give a direction."
  [player direction]
  (dosync
   (let [target-name ((:exits @(:current-room @player)) (keyword direction))
         target (rooms target-name)]
     (if target
       (do
         (util/move-between-refs (:name @player)
                                 @player
                                 (:inhabitants @(:current-room @player))
                                 (:inhabitants target))
         (ref-set (:current-room @player) target)
         (look player))
       "You can't go that way."))))

(defn grab
  "Pick something up."
  [player thing]
  (dosync
   (if (room-contains? @(:current-room @player) thing)
     (do (util/move-between-refs (keyword thing)
                                 ((keyword thing)
                                          @(:items @(:current-room @player)))
                                 (:items @(:current-room @player))
                                 (:inventory @player))
         (str "You picked up the " thing "."))
     (str "There isn't any " thing " here."))))

(defn discard
  "Put something down that you're carrying."
  [player thing]
  (dosync
   (if (carrying? thing player)
     (do (util/move-between-refs (keyword thing)
                                 ((keyword thing) @(:inventory @player))
                                 (:inventory @player)
                                 (:items @(:current-room @player)))
         (str "You dropped the " thing "."))
     (str "You're not carrying a " thing "."))))

(defn inventory
  "See what you've got."
  [player]
  (str "You are carrying:\n"
       (join "\n"  @(:inventory @player))))

(defn detect
  "If you have the detector, you can see which room an item is in."
  [player item]
  (if (@(:inventory @player) :detector)
    (if-let [room (first (filter #((:items %) (keyword item))
                                 (vals rooms)))]
      (str item " is in " (:name room))
      (str item " is not in any room."))
    "You need to be carrying the detector for that."))

(defn say
  "Say something out loud so everyone in the room can hear."
  [player & words]
  (let [message (join " " words)]
    (doseq [inhabitant
            (dissoc @(:inhabitants @(:current-room @player)) (:name @player))]
      (binding [*out* (@*player-streams* inhabitant)]
        (println message)
        (println (:prompt @player))))
    (str "You said " message)))

(defn help
  "Show available commands and what they do."
  [player]
  (join "\n" (map #(str (key %) ": " (:doc (meta (val %))))
                      (dissoc (ns-publics 'mire.commands)
                              'execute 'commands))))

;; Command data

(def commands {"move" move,
               "north" (fn [player] (move player :north)),
               "south" (fn [player] (move player :south)),
               "east" (fn [player] (move player :east)),
               "west" (fn [player] (move player :west)),
               "grab" grab
               "discard" discard
               "inventory" inventory
               "detect" detect
               "look" look
               "say" say
               "help" help})

;; Command handling

(defn execute
  "Execute a command that is passed to us."
  [player input]
  (try (let [[command & args] (.split input " +")]
         (apply (commands command) player args))
       (catch Exception e
         (.printStackTrace e *err*)
         "You can't do that!")))
