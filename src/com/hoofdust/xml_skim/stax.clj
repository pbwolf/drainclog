(ns com.hoofdust.xml-skim.stax
  "StAX helpers"
  (:import [javax.xml.stream 
            XMLStreamConstants XMLStreamReader]))

(defn burn-stax
  "Consumes events. Leaves the XMLStreamReader positioned so its
  current event is the END_ELEMENT for the context it was in
  initially.  Returns nil.  When the element contains only text,
  burn-stax is slower than XMLStreamReader.getElementText, but has the
  same net effect."
  [^XMLStreamReader xsr]
  (loop [levels 0]
    (.next xsr)
    (let [e (.getEventType xsr)]
      (cond
       (= e XMLStreamConstants/START_ELEMENT)
       (recur (inc levels))

       (= e XMLStreamConstants/END_ELEMENT)
       (when ( < 0 levels)
         (recur (dec levels))) ;; else exit!

       :else
       (recur levels)))))

(defn skip-non-tags
  "Leaves the XMLStreamReader positioned so its current event is the
  next START_ELEMENT or END_ELEMENT, which may leave its position
  absolutely unchanged.  Returns nil."
  [^XMLStreamReader xsr]
  (loop []
    (let [e (.getEventType xsr)]
      (cond
       (= e XMLStreamConstants/START_ELEMENT)
       nil

       (= e XMLStreamConstants/END_ELEMENT)
       nil

       :else
       (do
         (.next xsr)
         (recur))))))

(defmacro advance-xsr
  "Advances the XMLStreamReader to the next event and returns
  something true if there is a next event to advance to; otherwise
  returns nil."
  [xsr]
  `(when (.hasNext ~xsr)
     (.next ~xsr)
     true))
