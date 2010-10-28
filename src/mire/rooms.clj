(ns mire.rooms
  (:require [mire.items])
  (:import [mire.items Item]))

(defn- load-item-definitions
  "Create item records from maps stored in a room file.  Load
  them into the world items map."
  [items item-definitions]
  (alter
   items into 
   (map
    #(hash-map
      (:name %)
      (Item.
       (:name %) (:short-description %) (:long-description %)))
    item-definitions)))

(defn load-room
  "Load a single room from a file on disk.  Store room and item
  definitions in the supplied world map."
  [world file]
  (let [room (read-string (slurp (.getAbsolutePath file)))]
    (dosync
     (load-item-definitions (:items @world) (:item-definitions room))
     (alter (:rooms @world) conj
            {(keyword (.getName file))
             {:name (keyword (.getName file))
              :desc (:desc room)
              :exits (ref (:exits room))
              :items (ref (or (:items room) {}))
              :inhabitants (ref {})}}))))
  
(defn load-rooms
  "Given a dir, return a map with an entry corresponding to each file
  in it. Files should be maps containing room data."
  [world dir]
  (doall
   (map #(load-room world %) (.listFiles (java.io.File. dir)))))

(defn room-contains?
  [room thing]
  (@(:items room) (keyword thing)))
