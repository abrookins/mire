(ns mire.protocols)

(defprotocol Visible
  "A thing is visible in the world."
  (short-description [thing] "The one-line description of a thing.")
  (long-description [thing] "The longer description of a thing."))
