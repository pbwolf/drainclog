(ns com.hoofdust.xml-skim
  "Reads XML, described by a configuration of rules, into map
  structures. The main fn is pull-seq."
  (:import [javax.xml.stream 
            XMLStreamConstants XMLStreamReader])
  (:require [com.hoofdust.xml-skim.stax :as stax]
            [com.hoofdust.xml-skim.study :as study]))

(defn pull-object
  "Interprets XML stream until an object is complete. Returns a
  vector containing the object and the state with which next to call
  pull-object. Returns nil, instead of the vector, when the stream is
  over."
  [state]
  (when state
   (let [^XMLStreamReader xsr (get state :xsr)
         pstrategy (get state :path-strategy)]

     ;; This loop is a principal bottleneck.
     ;; We would like it to be as speedy as can be.
     ;; Therefore we rolled out an inner loop in the
     ;; start-element section.  

     (loop [stk (get state :stk)
            pps (get state :pps)]
       (let [e (.getEventType xsr)]
         (case e
           8 ;; XMLStreamConstants/END_DOCUMENT
           nil
         
           1 ;; XMLStreamConstants/START_ELEMENT
           (let [pp     (-> (peek pps)
                            (get :rpath)
                            (conj (.getLocalName xsr))
                            (pstrategy))
                 stk'   ((get pp :start-element) stk xsr)]
             ;; Avoid pushing/popping element level if no children:
             (if (get pp :depth-change)
               (do
                 (stax/skip-non-tags xsr)
                 (if (.isStartElement xsr)
                   (recur stk' (conj pps pp))
                   (if-let [f (get pp :end-element)]
                     (let [[stk'' eject] (f stk')]
                       (if eject
                         [eject (when (stax/advance-xsr xsr)
                                  (-> state
                                      (assoc :pps pps)
                                      (assoc :stk stk'')))]
                         (when (stax/advance-xsr xsr)
                           (recur stk'' pps))))
                     (when (stax/advance-xsr xsr)
                       (recur stk' pps)))))
               (recur stk' pps)))
         
           2 ;; XMLStreamConstants/END_ELEMENT
           (let [pps'   (pop pps)]
             (if-let [f (get (peek pps) :end-element)]
               (let [[stk' eject] (f stk)]
                 (if eject
                   [eject (when (stax/advance-xsr xsr)
                            (-> state
                                (assoc :pps pps')
                                (assoc :stk stk')))]
                   (when (stax/advance-xsr xsr)
                     (recur stk' pps'))))
               (when (stax/advance-xsr xsr)
                 (recur stk pps'))))

           ;; else
           (when (stax/advance-xsr xsr)
             (recur stk pps))))))))

(defn- start-pull
  "Rules - structure as illustrated in doc/sample.clj. Symbols - map of
  symbol to function, referred to by rule property complete-by."
  [rules symbols ^XMLStreamReader xsr]
  {:rtags '() :pps [] :stk [] :xsr xsr
   :path-strategy (study/path-disposal rules symbols)})

(defn pull-seq
  "Lazy sequence of objects pulled from the XML stream. Rules -
  structure as illustrated in doc/sample_configuration.clj. Symbols -
  map of symbol (used in the rule property, complete-by) to function
  that takes an object gleaned from the XML stream, transforms it in
  whatever way, and returns the object to save or eject."
  [rules symbols ^XMLStreamReader xsr]
  (letfn [(thatch-hedge [state]
            (lazy-seq 
             (when-let [[ob state'] (pull-object state)]
               (cons ob (thatch-hedge state')))))]
    (thatch-hedge (start-pull rules symbols xsr))))
