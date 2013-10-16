(ns com.hoofdust.xml-skim
  "Reads XML, described by rules, into structures"
  (:import [javax.xml.stream XMLStreamConstants XMLStreamReader])
  (:require [clojure.pprint :refer [pprint]] 
            [clojure.set :as set] 
            [clojure.string :as string]))

(defn prop-targets 
  "Set of :assign targets within a :text-value, :create, or attribute"
  [rules]
  (set (remove nil?
               (for [rule rules, 
                     kk `[[:create] 
                          [:text-value] 
                          ~@(for [a (-> rule :atts keys)] [:atts a])]]
                 (get-in rule `[~@kk :assign])))))

(defn rule-attr+text-targets 
  "For one rule, the set of :assign targets within a :text-value or attribute"
  [rule]
  (set (remove nil?
               (for [kk `[[:text-value] 
                          ~@(for [a (-> rule :atts keys)] [:atts a])]]
                 (get-in rule `[~@kk :assign])))))

(defn- assign-multi [c v]
  "assign-multi replaces the slower (fnil conj [])"
  (conj (or c []) v))

(defn start-element-pruning*
  [state]
  (let [^XMLStreamReader xsr (:xsr state)]
    (.next xsr)
    (loop [levels 0, e (.getEventType xsr)]
      (cond
       (= e XMLStreamConstants/START_ELEMENT)
       (recur (inc levels), (.next xsr))

       (= e XMLStreamConstants/END_ELEMENT)
       (when ( < 0 levels)
         (recur (dec levels) (.next xsr))) ;; nextTag does not save time
       
       :else
       (recur levels (.next xsr)))))
  (-> state
      (update-in [:rtags] pop)))

(defn start-element-atts*-+
  [state {:keys [sink] :as pp} ^XMLStreamReader xsr]
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

(defn end-element [state pp]
  (let [;;pp ((:path-strategy state) (:rtags state))
        ruleno (:ruleno pp) 
        end-el-f (when ruleno (get-in state [:rules ruleno :end-element]))]
    (-> state
        (cond-> end-el-f (end-el-f))
        (cond-> (:ob? pp) (update-in [:stk] pop))
        (update-in [:rtags] pop))))

(defn- analyze-rules-*-compose-end-element
  "Composes an :end-element handler for each rule, except :prune rules"
  [symbols cfg]
  (update-in 
   cfg [:rules]
   (fn [rules]
     (mapv (fn [rule] 
             (let [might-assign? (get-in rule [:create :assign])
                   eject-how  (get-in rule [:create :eject])]
               (if (or (:prune rule)
                       (not (or might-assign? eject-how)))
                 rule
                 (let [complete-f (some-> (get-in rule [:create :complete-by]) 
                                          (symbols))] 
                   (assoc 
                       rule :end-element
                       (fn [state]
                         (let [ob (some-> (peek (:stk state))
                                          (cond-> complete-f (complete-f)))
                               af (when might-assign? 
                                    ;; TBD why frequently lookup path-strategy?
                                    (let [pp ((:path-strategy state) (:rtags state))] 
                                      (:ob (:sink pp))))]
                           (-> state
                               (cond-> ob 
                                       (-> (cond-> af (af ob))
                                           (cond-> eject-how 
                                                   (assoc :eject ob)))))))))))) 
           rules))))

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

(defn path-disposal 
  "Function of rpath that yields a map containing:

  * :ruleno - the rule that applies to the rpath. May be nil.

  * :rulestk - stack (vector) of indexes into rules of the rule for
    this path and its ancestors.

  * :var-idx - property-key to rulestk-index map, considering the
    above rule's props, and all ancestor elements'.

  * :sink - map of attribute name, or :text or :ob, to a fn of state
    and a value, that either assigns the value to the property or
    conj's it if the property is a multi, and returns revised state."
  [rules]
  (let [rev-idx        (reverse-path-index rules) 
        assign-targets (prop-targets rules)
        memo           (atom {})] 
    (letfn [(updater [rules stk var-idx prop]
              {:pre [(vector? rules) 
                     (vector? stk) 
                     (map? var-idx)
                     (keyword? prop)]}
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
              (if (seq rpath)
                (let [parent        (disposal (drop 1 rpath))]
                  (if-let [ruleno   (ruleno-from-index rev-idx rpath)]
                    (let [p'rulestk (:rulestk parent)
                          frameno   (count p'rulestk)
                          rulestk   (conj p'rulestk ruleno)
                          rule      (get rules ruleno)
                          ob?       (boolean (:create rule))
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
                                    (apply merge {}))
                          ]
                      {:ob? ob?, :ruleno ruleno, 
                       :rulestk rulestk, :var-idx var-idx, :sink sink})
                    (assoc parent :ob? nil, :ruleno nil)))
                {:ob? nil, :ruleno nil, :rulestk [], :var-idx {}, :sink {}}))
            (disposal [rpath]
              (if-let [ret (@memo rpath)]
                ret
                (let [v (new-disposal rpath)]
                  (swap! memo assoc rpath v)
                  v)))]
      disposal)))

(defn- analyze-rules-*-compose-start-element
  "Composes a :start-element handler for each rule"
  [cfg]
  (let [assign-targets (prop-targets (:rules cfg))] 
    (update-in 
     cfg [:rules]
     (fn [rules]
       (mapv (fn [rule] 
               (assoc rule :start-element
                      (let [atts   (:atts rule)
                            ruleno (:ruleno rule)]
                        (cond
                         (and (:text-value rule) (:create rule))
                         (fn [state pp ^XMLStreamReader xsr]
                           (let [af  (-> pp :sink :text)]
                             (-> state
                                 (update-in [:stk] conj nil)
                                 (cond-> atts (start-element-atts*-+ pp xsr))
                                 (cond-> af   (af (.getElementText xsr)))
                                 (end-element pp))))

                         (and (:text-value rule) (not (:create rule)))
                         (fn [state pp ^XMLStreamReader xsr]
                           (let [af  (-> pp :sink :text)]
                             (-> state
                                 (cond-> atts (start-element-atts*-+ pp xsr))
                                 (cond-> af   (af (.getElementText xsr)))
                                 (end-element pp))))

                          (:prune rule)
                          (fn [state pp ^XMLStreamReader xsr]
                            (start-element-pruning* state))

                          (:create rule)
                          (fn [state pp ^XMLStreamReader xsr]
                            (-> state
                                (update-in [:stk] conj nil)
                                (cond-> atts (start-element-atts*-+ pp xsr))))

                          atts
                          (fn [state pp ^XMLStreamReader xsr]
                            (-> state
                                (start-element-atts*-+ pp xsr)))

                          :else
                          nil))))
             rules)))))

(defn analyze-rules
  [cfg symbols]
  (->> cfg
       (analyze-rules-*-compose-end-element symbols)
       (analyze-rules-*-compose-start-element)))

(defn start-element [{:keys [path-strategy rules] :as state}]
  (let [^XMLStreamReader xsr (:xsr state)
        rtags'  (conj (:rtags state) (.getLocalName xsr))
        pp      (path-strategy rtags')
        start-f (some-> (:ruleno pp)
                        (rules)
                        (:start-element))] 
    (-> state 
        (assoc :rtags rtags')
        (cond-> start-f (start-f pp xsr)))))

(defn start-pull
  "Rules - structure as illustrated in doc/sample.clj. Symbols - map of
  symbol to function, referred to by rule property complete-by. XSR -
  XmlStreamReader."
  [rules symbols ^XMLStreamReader xsr]
  ;; Note: Tried keeping keywords, not strings, in :rtags, but,
  ;; VisualVM said Symbol.intern was spending lots of time, and, sure
  ;; enough, the program got faster after abandoning keywordization.
  (-> {:rules rules :rtags '() :stk [] :xsr xsr
       :path-strategy (path-disposal rules)}
      (analyze-rules symbols)))

(defn pull-object
  "Interprets XML stream until an object is ready to eject. Returns a
  vector containing the object and the state with which next to call
  pull-object. Returns nil, instead of the vector, when the stream is
  over."
  [state]
  (let [^XMLStreamReader xsr (:xsr state) ] 
    (loop [state state]
      (let [e (.getEventType xsr)]
        (case e
         8 ;; XMLStreamConstants/END_DOCUMENT
         nil
         
         1 ;; XMLStreamConstants/START_ELEMENT
         (let [state' (start-element state)]
           (when (.hasNext xsr)
             (.next xsr))
           (recur state'))
         
         2 ;; XMLStreamConstants/END_ELEMENT
         (let [state' (end-element state 
                                   ((:path-strategy state) (:rtags state)))]
           (when (.hasNext xsr)
             (.next xsr))
           (if-let [eject (:eject state')] 
             [eject (dissoc state' :eject)]
             (recur state')))
         
         ;; else
         (do
           (when (.hasNext xsr)
            (.next xsr))
           (recur state)))))))

(defn pull-seq
  "Lazy sequence of objects pulled from the XML stream"
  [rules symbols ^XMLStreamReader xsr]
  (letfn [(pull-seq* [state]
            (lazy-seq 
             (when-let [[ob state'] (pull-object state)]
               (cons ob (pull-seq* state')))))]
    (pull-seq* (start-pull rules symbols xsr))))
