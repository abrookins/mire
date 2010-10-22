(ns mire.items
  (:use [mire.protocols :as protocols])
  (:use [clojure.contrib.string :only [join]]))

(def *items* (ref {}))

(defrecord Item [name short-description long-description]
  protocols/Visible
  (short-description
   [item]
   (str "There is " (:short-description item) " here.\n"))
   (long-description
   [item]
   (str (:long-description item) ".\n")))
