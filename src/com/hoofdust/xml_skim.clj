(ns com.hoofdust.xml-skim
  "Reads XML, described by a configuration of rules, into map
  structures. The main fn is pull-seq."
  (:import [javax.xml.stream 
            XMLStreamConstants XMLStreamReader])
  (:require [clojure.set :as set] 
            [clojure.string :as string]
            [clojure.core.match :as match]))

(defn assign-atts
  "Given a state accumulator, an XMLStreamReader that is positioned at
  a start-element that might have attributes, and a map of
  attribute-name to fn of state accumulator and new value, thread the
  state accumulator through the fns for the element's actual
  attribute values."
  [state, sink, ^XMLStreamReader xsr]
  (loop [state state, i (dec (.getAttributeCount xsr))]
    (if (neg? i)
      state
      (recur 
       (or (some-> 
            (.getAttributeLocalName xsr i)
            (sink)
            (as-> X (X state (.getAttributeValue xsr i))))
           state)
       (dec i)))))

(defn prop-targets 
  "Set of :assign targets within a :text-value, :create, or attribute"
  [rules]
  (set (remove nil?
               (for [rule rules, 
                     kk `[[:create] 
                          [:text-value] 
                          ~@(for [a (-> rule :atts keys)] [:atts a])]]
                 (get-in rule `[~@kk :assign])))))

(defn reverse-path-index
  "Structure for ruleno-from-index to use"
  [rules]
  (-> rules
      (->> (map #(-> % :path (string/split #"/") reverse (concat [:end]))))
      (zipmap (range))
      (->> (reduce-kv assoc-in {}))))

(defn ruleno-from-index
  "Given reverse-path index and a reverse path, trace the path into the
  index far enough to get an integer. Nil if none."
  [idx rpath]
  (when-let [t (first rpath)]
    (if-let [next-idx (get idx t)]
      (if-let [deeper-answer (ruleno-from-index next-idx (rest rpath))]
        deeper-answer
        (:end next-idx)))))

(defn- assign-multi [c v]
  "assign-multi replaces the slower (fnil conj [])"
  (conj (or c []) v))

(defn burn-stax
  "With the XMLStreamReader initially parked at a start-element event,
  consume events until it is parked at the corresponding end-element.
  Returns nil."
  [^XMLStreamReader xsr]
  (.next xsr)
  (loop [levels 0, e (.getEventType xsr)]
    (cond
     (= e XMLStreamConstants/START_ELEMENT)
     (recur (inc levels), (.next xsr))

     (= e XMLStreamConstants/END_ELEMENT)
     (when ( < 0 levels)
       (recur (dec levels) (.next xsr))) 
     
     :else
     (recur levels (.next xsr)))))

(defmacro advance-xsr 
  "Advances the XMLStreamReader to the next event and returns state if
  there is a next event to advance to; otherwise returns nil"
  [state xsr]
  `(let [s# ~state] 
     (when (.hasNext ~xsr)
       (.next ~xsr) 
       s#)))

(defn end-element-f
  [symbols rules pp]
  (let [rule (get rules (:ruleno pp) nil)
        criteria [(when (:create rule) 
                    :create) 
                  (when-let [f (some-> (get-in rule [:create :complete-by]) 
                                       (symbols))]
                    [f])
                  (when-let [s (get-in pp [:sink :ob])]
                    [s])
                  (when-let [e (get-in rule [:create :eject])]
                    [e])]]
    (match/match 
     criteria

     [nil _ _ _]
     nil

     [:create _ nil nil]
     (fn [state]
       (-> state
           (update-in [:stk] pop)))
     
     [:create nil nil [e]]
     (fn [state]
       (let [ob (peek (:stk state))] 
         (-> state
             (update-in [:stk] pop)
             (cond-> ob (assoc :eject ob)))))
     
     [:create nil [s] nil]
     (fn [state]
       (let [ob (peek (:stk state))] 
         (-> state
             (update-in [:stk] pop)
             (cond-> ob (s ob)))))

     [:create nil [s] [e]]
     (fn [state]
       (let [ob (peek (:stk state))] 
         (-> state
             (update-in [:stk] pop)
             (cond-> ob (-> (s ob) (assoc :eject ob))))))
     
     [:create [f] nil [e]]
     (fn [state]
       (let [ob (some-> (peek (:stk state))
                        (f))] 
         (-> state
             (update-in [:stk] pop)
             (cond-> ob (assoc :eject ob)))))
     
     [:create [f] [s] nil]
     (fn [state]
       (let [ob (some-> (peek (:stk state))
                        (f))] 
         (-> state
             (update-in [:stk] pop)
             (cond-> ob (s ob)))))
     
     [:create [f] [s] [e]]
     (fn [state]
       (let [ob (some-> (peek (:stk state))
                        (f))] 
         (-> state
             (update-in [:stk] pop)
             (cond-> ob (-> (s ob) (assoc :eject ob)))))))))

(defn start-element-f
  "Two-item vector - first, a boolean indicating whether the
  start-element handler produces a net change in the element nesting
  level (It does not change the nesting level if it consumes not only
  the start-element event, but also any text and the ensuing
  end-element event); and second, a function, of state and
  XMLStreamReader, to adjust the state upon a start-element event and
  advance the XMLStreamReader to the next event."
  [rules pp]
  (let [rule   (get rules (:ruleno pp) nil) 
        atts   (:atts rule)
        sink   (:sink pp) 
        text-sink (:text sink)
        
        ;;criteria [create? atts? content?]
        criteria [(when (:create rule)     :create)
                  (when (:atts rule)       :atts)
                  (cond (:text sink)       :text-keep 
                        (:text-value rule) :text-drop
                        (:prune rule)      :prune
                        :else              :children )]]
    (match/match
        criteria
      
      [:create :atts :text-keep]
      [true (fn [state ^XMLStreamReader xsr]
               (-> state
                   (update-in [:stk] conj nil)
                   (assign-atts sink xsr)
                   (text-sink (.getElementText xsr))))]                   
      
      [:create :atts :text-drop]
      [true (fn [state ^XMLStreamReader xsr]
              (.getElementText xsr) ;; Side effects XSR posn
              (-> state
                  (update-in [:stk] conj nil)
                  (assign-atts sink xsr)))]
      
      [:create nil :text-keep]
      [true (fn [state ^XMLStreamReader xsr]
               (-> state
                   (update-in [:stk] conj nil)
                   (text-sink (.getElementText xsr))))]
      
      [ _ nil :text-drop]
      [false (fn [state ^XMLStreamReader xsr]
              (.getElementText xsr) ;; Do not skip. Advances XSR to End.
              (-> state
                  (advance-xsr xsr)))]
      
      [nil :atts :text-keep]
      [true (fn [state ^XMLStreamReader xsr]
              (-> state
                  (assign-atts sink xsr)
                  (text-sink (.getElementText xsr))))]                   
      
      [nil nil :text-keep]
      [true (fn [state ^XMLStreamReader xsr]
              (-> state
                  (text-sink (.getElementText xsr))))]                   
      
      [nil :atts :text-drop]
      [true (fn [state ^XMLStreamReader xsr]
              (.getElementText xsr) ;; Do not skip. Advances XSR to End.
              (-> state
                  (assign-atts sink xsr)))]
      
      [nil nil :prune]
      [false (fn [state ^XMLStreamReader xsr]
               (burn-stax xsr)
               (advance-xsr state xsr))]
      
      [:create nil :children]
      [true (fn [state ^XMLStreamReader xsr]
              (-> state
                  (update-in [:stk] conj nil)
                  (advance-xsr xsr)))]
      
      [:create :atts :children]
      [true (fn [state ^XMLStreamReader xsr]
              (-> state
                  (update-in [:stk] conj nil)
                  (assign-atts (:sink pp) xsr)
                  (advance-xsr xsr)))]
      
      [nil :atts :children]
      [true (fn [state ^XMLStreamReader xsr]
              (-> state 
                  (assign-atts (:sink pp) xsr)
                  (advance-xsr xsr)))]
      
      [nil nil :children]
      [true (fn [state ^XMLStreamReader xsr]
              (-> state 
                  (advance-xsr xsr)))]
      
      :else
      (throw (RuntimeException. (str "What to do with " criteria))))))

(defn path-disposal 
  "Function of rpath that yields a map containing:

  * :ruleno - the rule that applies to the rpath. May be nil.

  * :rulestk - stack (vector) of indexes into rules of the rule for
    this path and its ancestors.

  * :var-idx - property-key to rulestk-index map, considering the
    above rule's props, and all ancestor elements'.

  * :sink - map of attribute name, or :text or :ob, to a fn of state
    and a value, that either assigns the value to the property or
    conj's it if the property is a multi, and returns revised state.

  * :start-element - function of state and XMLStreamReader that 
    adjusts the state in response to a start-element event 
    and advances the XMLStreamReader"
  [rules symbols]
  (let [rev-idx        (reverse-path-index rules) 
        assign-targets (prop-targets rules)
        memo           (atom {})] 
    (letfn [(updater [rules stk var-idx prop]
              (when-let [frame-no (var-idx prop)]
                (let [vruleno (get stk frame-no)
                      rule    (rules vruleno)
                      propdef (-> rule :create :props prop)
                      multi?  (:multi propdef)
                      route   [:stk frame-no prop]]
                  (if multi?
                    (fn u9 [state v] (update-in state route assign-multi v))
                    (fn a1 [state v] (assoc-in state route v))))))
            (new-disposal [rpath]
              {:post [(vector? (:rulestk %))
                      (map? (:sink %))]}
              (->
               (if (seq rpath)
                 (let [parent        (disposal (drop 1 rpath))]
                   (if-let [ruleno   (ruleno-from-index rev-idx rpath)]
                     (let [p'rulestk (:rulestk parent)
                           frameno   (count p'rulestk)
                           rulestk   (conj p'rulestk ruleno)
                           rule      (get rules ruleno)
                           props     (seq (filter assign-targets 
                                                  (-> rule :create :props keys)))
                           ob-prop   (get-in rule [:create :assign])
                          
                           t-prop    (-> rule :text-value :assign)
                          
                           p'var-idx (:var-idx parent)
                           var-idx   (merge p'var-idx
                                            (zipmap props (repeat frameno)))
                          
                           sink (->> (apply concat
                                            (when ob-prop 
                                              [[:ob p'var-idx ob-prop]])
                                            (when t-prop 
                                              [[:text var-idx t-prop]])
                                            (for [[att {prop :assign}] (:atts rule)
                                                  :when prop] 
                                              [[att var-idx prop]]))
                                     (map (fn [[k v p]]
                                            (when-let 
                                                [u (updater rules rulestk v p)]
                                              {k u})))
                                     (apply merge {})) ]
                       {:ruleno ruleno, 
                        :rulestk rulestk, :var-idx var-idx, :sink sink})
                     (assoc parent :ruleno nil)))
                 {:ruleno nil, :rulestk [], :var-idx {}, :sink {}})

               (as-> x (merge x
                              (zipmap [:depth-change :start-element] 
                                      (start-element-f rules x))))
               (as-> x (assoc x :end-element (end-element-f symbols rules x)))))
            (disposal [rpath]
              (if-let [ret (@memo rpath)]
                ret
                (let [v (new-disposal rpath)]
                  (swap! memo assoc rpath v)
                  v)))]
      disposal)))

(defn pull-object
  "Interprets XML stream until an object is complete. Returns a
  vector containing the object and the state with which next to call
  pull-object. Returns nil, instead of the vector, when the stream is
  over."
  [state]
  (when state
   (let [^XMLStreamReader xsr (:xsr state) 
         pstrategy (:path-strategy state)] 
     (loop [state state rtags (:rtags state) pps (:pps state)]
       (let [e (.getEventType xsr)]
         ;; You might factor out the when-hasNext-next, but if you
         ;; consequently factor out the conditional eject and apply it
         ;; to events of all types, everything gets slower.
         (case e
           8 ;; XMLStreamConstants/END_DOCUMENT
           nil
         
           1 ;; XMLStreamConstants/START_ELEMENT
           (let [rtags' (conj rtags (.getLocalName xsr))
                 pp     (pstrategy rtags')
                 state' ((:start-element pp) state xsr)]
             (recur state' 
                    (if (:depth-change pp) rtags' rtags)
                    (if (:depth-change pp) (conj pps pp) pps)))
         
           2 ;; XMLStreamConstants/END_ELEMENT
           (let [pp     (peek pps)
                 state' (if-let [f (:end-element pp)  ] 
                          (f state)
                          state)
                 rtags' (pop rtags)
                 pps'   (pop pps)]
             (if-let [eject (:eject state')] 
               [eject (-> state'
                          (dissoc :eject)
                          (assoc :rtags rtags')
                          (assoc :pps pps')
                          (advance-xsr xsr))]
               (when-let [state'' (advance-xsr state' xsr)] 
                 (recur state'' rtags' pps'))))
         
           ;; else
           (when-let [state' (advance-xsr state xsr)] 
             (recur state rtags pps))))))))

(defn start-pull
  "Rules - structure as illustrated in doc/sample.clj. Symbols - map of
  symbol to function, referred to by rule property complete-by."
  [rules symbols ^XMLStreamReader xsr]
  {:rtags '() :pps [] :stk [] :xsr xsr
   :path-strategy (path-disposal rules symbols)})

(defn pull-seq
  "Lazy sequence of objects pulled from the XML stream. Rules -
  structure as illustrated in doc/sample_configuration.clj. Symbols -
  map of symbol (used in the rule property, complete-by) to function."
  [rules symbols ^XMLStreamReader xsr]
  (letfn [(thatch-hedge [state]
            (lazy-seq 
             (when-let [[ob state'] (pull-object state)]
               (cons ob (thatch-hedge state')))))]
    (thatch-hedge (start-pull rules symbols xsr))))


  ;; Note: Tried keeping keywords, not strings, in :rtags, but,
  ;; VisualVM said Symbol.intern was spending lots of time, and, sure
  ;; enough, the program got faster after abandoning keywordization.


        ;; It seemed that 'case' with constants ran a tiny bit faster
        ;; than 'cond' with symbols.  Not worth it, I guess.

