(ns mire.commands
  (:use [mire.rooms :only [room-contains?]]
        [mire.player :only [carrying?]]
        [mire.protocols :as protocols]
        [mire.util :as util])
  (:use [clojure.contrib.string :only [join]]))

(defn -short-description
  "Call short-description on each item in a collection.  Call
  lookup-fn to find the target item given a key."
  [lookup-fn col]
  (join "\n" (map #(short-description (lookup-fn %)) col)))

;; Command functions

(defn look "Get a description of the surrounding environs and its contents."
  ;; TODO: look [object]
  [player world]
  (str (:desc @(:current-room @player))
       "\nExits: " (keys @(:exits @(:current-room @player))) "\n"
       (-short-description
        #(@(:items @world) %) (keys @(:items @(:current-room @player))))
       (-short-description ;; TODO: Players should follow items pattern.
        #(identity %) (vals @(:inhabitants @(:current-room @player))))))

(defn move
  "\"♬ We gotta get out of this place... ♪\" Give a direction."
  [player world direction]
  (dosync
   (let [target-name ((:exits @(:current-room @player)) (keyword direction))
         target ((:rooms @world) target-name)]
     (if target
       (do
         (util/move-between-refs (:name @player)
                                 @player
                                 (:inhabitants @(:current-room @player))
                                 (:inhabitants target))
         (ref-set (:current-room @player) target)
         (look player world))
       "You can't go that way."))))

(defn- make-move-fn
  "Make a movement function given a direction."
  [direction]
  (fn [player world] (move player world direction)))

(defn grab
  "Pick something up."
  [player _ thing]
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
  [player _ thing]
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
  [player _]
  (str "You are carrying:\n"
       (join "\n"  @(:inventory @player))))

(defn detect
  "If you have the detector, you can see which room an item is in."
  [player world item]
  (if (@(:inventory @player) :detector)
    (if-let [room (first (filter #((:items %) (keyword item))
                                 (vals (:rooms world))))]
      (str item " is in " (:name room))
      (str item " is not in any room."))
    "You need to be carrying the detector for that."))

(defn say
  "Say something out loud so everyone in the room can hear."
  [player world & words]
  (let [message (join " " words)]
    (doseq [inhabitant
            (dissoc @(:inhabitants @(:current-room @player)) (:name @player))]
      (binding [*out* (@(:player-streams @world) inhabitant)]
        (println message)
        (println (:prompt @player))))
    (str "You said " message)))

(defn help
  "Show available commands and what they do."
  [player world]
  (join "\n" (map #(str (key %) ": " (:doc (meta (val %))))
                      (dissoc (ns-publics 'mire.commands)
                              'execute 'commands))))

;; Command data

(def commands {"move" move,
               "north" (make-move-fn :north),
               "south" (make-move-fn :south),
               "east" (make-move-fn :east),
               "west" (make-move-fn :west),
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
  [player world input]
  (try (let [[command & args] (.split input " +")]
         (apply (commands command) player world args))
       (catch Exception e
         (.printStackTrace e *err*)
         "You can't do that!")))
