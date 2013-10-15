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

;; is this better/worse/same as (fnil conj [])
(defn- assign-multi [c v]
  (conj (or c []) v))

(defn- assign-1 [c v]
  v)

(defn- update-with 
  "Like update-in, but takes keys and function in a vector"
  [a [ks uf] v]
  (update-in a ks uf v))

;; Here are some composable aspects of start-element and end-element handlers.  
;; The rule analyzer includes aspects required by the rules.
;; The start-element handler may prescribe a nested event loop,
;; so predeclare the event loops:

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
  [state {:keys [sink] :as pp}]
  (let [^XMLStreamReader xsr (:xsr state)]
    (loop [state state, i (dec (.getAttributeCount xsr))]
      (if (neg? i)
        state
        (recur 
         (or (some-> 
              (.getAttributeLocalName xsr i)
              (sink)
              (as-> X (X state (.getAttributeValue xsr i))))
             state)
         (dec i))))))

(defn end-element-complete*-
  [{:keys [stk] :as state} complete-f]
  (let [frame (peek stk)]
    (if-let [ob (:ob frame)]
      (assoc-in state [:stk (dec (count stk)) :ob] (complete-f ob))
      state)))


(defn end-element-assign-ob*-+
  [{:keys [stk] :as state} pp]
  {:pre [(map? (:sink pp))]}
  (if-let [ob (:ob (peek stk))]
    (do
      (if-let [af (:ob (:sink pp))]
        (af state ob)
        state))
    state))

(defn end-element-eject-ob*-
  [{:keys [stk] :as state} eject-how]
  (if-let [ob (:ob (peek stk))]
    (if ( = true eject-how) 
      (assoc state :eject ob)
      (do (eject-how ob) 
          state))
    state))

(defn end-element [state]
  (let [end-el-f (-> state :stk peek :end-element) ]
    (-> state
        (cond-> end-el-f (end-el-f))
        (update-in [:stk] pop)
        (update-in [:rtags] pop))))

(defn- analyze-rules-*-compose-end-element
  "Composes an :end-element handler for each rule, except :prune rules"
  [symbols cfg]
  (update-in 
   cfg [:rules]
   (fn [rules]
     (mapv (fn [rule] 
             (if (:prune rule)
               rule
               (let [complete-f (some-> (get-in rule [:create :complete-by]) 
                                        (symbols))
                     o-p        (get-in rule [:create :assign])
                     eject-how  (get-in rule [:create :eject])] 
                 (assoc rule :end-element
                        (fn [state]
                          (let [pp ((:path-strategy state) (:rtags state))]
                            (-> state
                                (cond-> complete-f 
                                        (end-element-complete*- complete-f))
                                (cond-> o-p 
                                        (end-element-assign-ob*-+ pp))
                                (cond-> eject-how 
                                        (end-element-eject-ob*- eject-how))))))))) 
           rules))))

(defn stax-nab-text!- [state {:keys [sink] :as pp}]
  (let [^XMLStreamReader xsr (:xsr state) 
        af (sink :text)]
    (cond-> state 
            af (af (.getElementText xsr)))))

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
                      rule   (rules vruleno)
                      propdef (-> rule :create :props prop)
                      multi? (:multi propdef)
                      route [:stk frame-no :ob prop]]
                  (if multi?
                    (fn u9 [state v] (update-in state route assign-multi v))
                    (fn a1 [state v] (assoc-in state route v))))))
            (new-disposal [rpath]
              {:post [(vector? (:rulestk %))
                      (map? (:sink %))]}
              (if (seq rpath)
                (let [frameno   (dec (count rpath)) 
                      parent    (disposal (drop 1 rpath))
                      p'rulestk (:rulestk parent)
                      ruleno    (ruleno-from-index rev-idx rpath)
                      rulestk   (conj p'rulestk ruleno)
                      rule      (get rules ruleno)
                      props     (seq (filter assign-targets 
                                             (-> rule :create :props keys)))
                      ob-prop   (get-in rule [:create :assign])
                      up-targets
                      (->> (cons
                            ob-prop
                            (set/difference (rule-attr+text-targets rule)
                                            (set props)))
                           (remove nil?))
                      
                      t-prop    (-> rule :text-value :assign)

                      up-targets-set (set up-targets)
                      
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
                  {:rulestk rulestk, :var-idx var-idx, :sink sink})
                {:rulestk [], :var-idx {}, :sink {}}))
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
                            ruleno (:ruleno rule)
                            end-el-f (:end-element rule)]
                        (cond
                         (:text-value rule)
                         (fn [state]
                           (let [pp ((:path-strategy state) (:rtags state))]
                             ;; text guarantees no child elts.
                             ;; Need stk frame only if creating new object.
                             ;; And then only b/c too confusing otherwise.
                             ;; There might be atts.
                             (-> state
                                 (update-in [:stk] conj 
                                            {:end-element end-el-f})
                                 (cond-> atts (start-element-atts*-+ pp))
                                 (stax-nab-text!- pp)
                                 (end-element))))

                          (:prune rule)
                          (fn [state]
                            (start-element-pruning* state))

                          :else
                          (fn [state]
                            (let [pp ((:path-strategy state) (:rtags state))]
                              (-> state
                                  (update-in [:stk] conj 
                                             {:end-element end-el-f})
                                  (cond-> atts (start-element-atts*-+ pp)))))))))
             rules)))))

(defn analyze-rules-*-reverse-path-index
  [cfg]
  (assoc cfg :rev-path-to-ruleno (reverse-path-index (:rules cfg))))

(defn analyze-rules
  [rules symbols]
  (->> {:rules rules}
       (analyze-rules-*-compose-end-element symbols)
       (analyze-rules-*-compose-start-element)
       (analyze-rules-*-reverse-path-index)))

(defn start-element-rule
  "Identifies rule suitable for state's :rtags. Pushes stack frame."
  [{:keys [rev-path-to-ruleno rtags] :as state}]
  (if-let [ruleno (ruleno-from-index rev-path-to-ruleno rtags)]
    (let [start-el-f (get-in state [:rules ruleno :start-element])]
      (start-el-f state))
    (-> state 
        (update-in [:stk] conj nil))))

(defn start-element [state]
  (let [^XMLStreamReader xsr (:xsr state)] 
    (-> state 
        (update-in [:rtags] conj (.getLocalName xsr))
        (start-element-rule))))

(defn pull-parse-event-advance
  "Computes new state, based on old state and event, ejects the
  object that the event completes (if any), and advances xsr to the
  next event. Returns new state, or nil if document is over."
  [state]
  (let [^XMLStreamReader xsr (:xsr state) 
        state' (let [e (.getEventType xsr)]
                 (cond
                  (= e XMLStreamConstants/END_DOCUMENT)
                  nil
                  
                  (= e XMLStreamConstants/START_ELEMENT)
                  (start-element state)
                  
                  (= e XMLStreamConstants/END_ELEMENT)
                  (end-element state)
                  
                  :else
                  state))]
    (when state'
      (assert (identical? xsr (:xsr state')))
      (when (.hasNext xsr)
        (.next xsr)  ;; .nextTag, on endElement, in files with no mixed content, saves a tiny amount of time
        state'))))

(defn start-pull
  "Rules - structure as illustrated in doc/sample.clj. Symbols - map of
  symbol to function, referred to by rule property complete-by. XSR -
  XmlStreamReader."
  [rules symbols ^XMLStreamReader xsr]
  ;; Note: Tried keeping keywords, not strings, in :rtags, but,
  ;; VisualVM said Symbol.intern was spending lots of time, and, sure
  ;; enough, the program got faster after abandoning keywordization.
  (-> rules 
      (analyze-rules symbols)
      (assoc :rtags '())
      (assoc :stk [])
      (assoc :xsr xsr)
      (as-> x (assoc x :path-strategy (path-disposal (:rules x))))))

(defn pull-object
  "Interprets XML stream until an object is ready to eject. Returns a
  vector containing the object and the state with which next to call
  pull-object. Returns nil, instead of the vector, when the stream is
  over."
  [state]
  (loop [state state]
    (when-let [state' (pull-parse-event-advance state)]
      (if-let [eject (:eject state')] 
        (do 
          (when (:warning-pruned state')
            (println "Warning: Pruned useless paths: " 
                     (:warning-pruned state')))
          [eject (dissoc state' :eject :warning-pruned)])
        (recur state')))))

(defn pull-seq
  "Lazy sequence of objects pulled from the XML stream"
  [rules symbols ^XMLStreamReader xsr]
  (letfn [(pull-seq* [state]
            (lazy-seq 
             (when-let [[ob state'] (pull-object state)]
               (cons ob (pull-seq* state')))))]
    (pull-seq* (start-pull rules symbols xsr))))
