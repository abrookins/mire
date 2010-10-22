(ns mire.rooms
  (:require [mire.items])
  (:import [mire.items Item]))

(def rooms {})

(defn- load-items
  "Create item records from maps stored in a room file."
  [items]
  (map #(Item. (:name %) (:short-description %) (:long-description %)) items))

(defn load-room [rooms file]
  (let [room (read-string (slurp (.getAbsolutePath file)))]
    (conj rooms
          {(keyword (.getName file))
           {:name (keyword (.getName file))
            :desc (:desc room)
            :exits (ref (:exits room))
            :items (ref (or (load-items (:items room)) []))
            :inhabitants (ref {})}})))
  
(defn load-rooms
  "Given a dir, return a map with an entry corresponding to each file
  in it. Files should be maps containing room data."
  [rooms dir]
  (reduce load-room rooms (.listFiles (java.io.File. dir))))

(defn add-rooms
  "Look through all the files in a dir for files describing rooms and add
  them to the mire.rooms/rooms map."
  [dir]
  (alter-var-root #'rooms load-rooms dir))

(defn room-contains?
  [room thing]
  (@(:items room) (keyword thing)))
