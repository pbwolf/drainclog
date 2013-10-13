(ns com.hoofdust.xml-skim
  "Reads XML, described by rules, into structures"
  (:import [javax.xml.stream XMLStreamConstants XMLStreamReader])
  (:require [clojure.set :as set] 
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

(defn- assign-1 [c v]
  v)

(defn- assign-multi [c v]
  (conj (or c []) v))

(defn- update-with 
  "Like update-in, but takes keys and function in a vector"
  [a [ks uf] v]
  (update-in a ks uf v))

(defn assign-vec 
  "Figures out how to assign to a property. Returns a vector of two
  elements: an update-in vector for the parse state, and a combining
  fn that reflects whether the property is to be multi-valued. Nil,
  instead of the vector, if the prop can't be assigned."
  [rules stk var-frames prop]
  (when-let [frame-no (var-frames prop)]
    (let [frame (get stk frame-no)
          rule  (get rules (:ruleno frame))
          propdef (-> rule :create :props prop)
          multi? (:multi propdef)]
     [[:stk frame-no :ob prop] (if multi? assign-multi assign-1)])) )

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

;; start-element handlers:

;; variants:
;;  (1) create object or not?
;;  (2) attend to attributes or not?
;;  (3) with contents:  prune or text or children?
;; that's... 12? variants.


(defn start-element-textbuf*
  [{:keys [stk] :as state}]
  (-> state 
      (assoc-in [:stk (dec (count stk)) :text-buf] (StringBuilder.))))

(defn start-element-novars*
  [state]
  (let [var-frames (or (peek (:var-idx state)) {})]
    (update-in state [:var-idx] conj var-frames)))

(defn start-element-vars*-
  [state rule-props-to-index]
  (let [stk (:stk state)
        last-frame-no (dec (count stk))
        var-frames (peek (:var-idx state))]
    (update-in state [:var-idx] conj 
               (merge var-frames 
                      (zipmap rule-props-to-index (repeat last-frame-no))))))

(defn start-element-atts*-
  [{:keys [rules stk] :as state} att-defs]
  (let [^XMLStreamReader xsr (:xsr state)
        var-frames (peek (:var-idx state))]
    (loop [state state, i (dec (.getAttributeCount xsr))]
      (if (neg? i)
        state
        (recur 
         (or (some-> 
              (.getAttributeLocalName xsr i)
              (as-> X (get-in att-defs [X :assign]))
              (as-> X (assign-vec rules stk var-frames X))
              (as-> X (update-with state X (.getAttributeValue xsr i))))
             state)
         (dec i))))))

(defn end-element-varidx
  [state]
  (update-in state [:var-idx] pop))

(defn end-element-complete*
  [complete-f, {:keys [stk] :as state}]
  (let [frame (peek stk)]
    (if-let [ob (:ob frame)]
      (assoc-in state [:stk (dec (count stk)) :ob] (complete-f ob))
      state)))

(defn end-element-assign-ob*
  [o-p, {:keys [stk rules var-idx] :as state}]
  (if-let [ob (:ob (peek stk))]
    (if-let [[o-uks o-uf] (assign-vec rules (pop stk) (peek var-idx) o-p)]
      (update-in state o-uks o-uf ob)
      state)
    state))

(defn end-element-eject-ob*
  [eject-how, {:keys [stk] :as state}]
  (if-let [ob (:ob (peek stk))]
    (if ( = true eject-how) 
      (assoc state :eject ob)
      (do (eject-how ob) 
          state))
    state))

(def end-element-dflts end-element-varidx)

(defn end-element [state]
  (let [end-el-f (or (-> state :stk peek :end-element)
                     end-element-dflts)]
    (-> state
        (end-el-f)
        (update-in [:stk] pop)
        (update-in [:rtags] pop))))

(defn- analyze-rules-*-number 
  "Adds :ruleno to each rule, to facilitate identification for random access"
  [cfg]
  (update-in 
   cfg [:rules] (fn [rules] 
                  (vec
                   (map-indexed (fn [i rule] (assoc rule :ruleno i)) rules)))))

(defn- analyze-rules-*-compose-end-element
  "Composes an :end-element handler for each rule, except :prune rules"
  [symbols cfg]
  (update-in 
   cfg [:rules]
   (fn [rules]
     (mapv (fn [rule] 
            (if (:prune rule)
              rule
              (->> [
                    ;; Steps for an end-element handler:

                    [end-element-varidx]

                    (when-let [complete-f 
                               (some-> (get-in rule [:create :complete-by])
                                       (symbols))]
                      [(partial end-element-complete* complete-f)])

                    (when-let [o-p (get-in rule [:create :assign])]
                      [(partial end-element-assign-ob* o-p)])

                    (when-let [eject-how (get-in rule [:create :eject])]
                      [(partial end-element-eject-ob* eject-how)])

                    ]
                   
                   (apply concat)
                   (reverse) ;; because comp runs fns right-to-left
                   (apply comp)
                   (assoc rule :end-element)))) 
          rules))))

(defn stax-nab-text! [{:keys [stk rules var-idx] :as state} t-p]
  (let [^XMLStreamReader xsr (:xsr state) 
        stuff (.getElementText xsr)
        [t-uks t-uf] (assign-vec rules stk (peek var-idx) t-p)]
    (cond-> state t-uks (update-in t-uks t-uf stuff))))

(defn- analyze-rules-*-compose-start-element
  "Composes a :start-element handler for each rule"
  [cfg]
  (let [assign-targets (prop-targets (:rules cfg))] 
    (update-in 
     cfg [:rules]
     (fn [rules]
       (mapv (fn [rule] 
               (assoc rule :start-element
                      (let [rule-props-to-index
                            (when-let [props (-> rule :create :props keys set)]
                              (seq (set/intersection assign-targets props)))
                            
                            external-targets
                            (->> (concat
                                  [(get-in rule [:create :assign])]
                                  (set/difference
                                   (rule-attr+text-targets rule)
                                   (set (-> rule :create :props keys))))
                                 (remove nil?))
                            
                            external-targets-set (set external-targets)

                            create (when (:create rule) :create)
                            atts   (:atts rule)
                            content (cond 
                                     (:prune rule)      :prune
                                     (:text-value rule) :text
                                     :else              :children) 
                            ejecting? (-> rule :create :eject)
                            
                            ruleno (:ruleno rule)
                            end-el-f (:end-element rule)

                            t-p (-> rule :text-value :assign)
                            ]
                        (when-not (or ejecting?
                                      (:prune rule) 
                                      (seq external-targets))
                          (throw (RuntimeException. 
                                  (str "This rule targets nothing: " rule))))
                        (fn [state]
                          (let [ext-varidx (peek (:var-idx state))]
                            (condp = content
                              :text 
                              (do 
                                ;; text guarantees no child elts.
                                ;; Need stk frame only if creating new object.
                                ;; And then only b/c too confusing otherwise.
                                ;; There might be atts.
                                (if rule-props-to-index
                                  (-> state
                                      (update-in [:stk] conj 
                                                 {:ruleno ruleno, 
                                                  :end-element end-el-f})
                                      (start-element-vars*- rule-props-to-index)
                                      (cond-> atts (start-element-atts*- atts))
                                      (stax-nab-text! t-p)
                                      (end-element))
                                  (-> state
                                      (update-in [:stk] conj 
                                                 {:ruleno ruleno, 
                                                  :end-element end-el-f})
                                      (update-in [:var-idx] conj
                                                 (or ext-varidx {}))
                                      (cond-> atts (start-element-atts*- atts))
                                      (stax-nab-text! t-p)
                                      (end-element))))
                              :children 
                              (do 
                                (if rule-props-to-index
                                  (-> state
                                      (update-in [:stk] conj 
                                                 {:ruleno ruleno, 
                                                  :end-element end-el-f})
                                      (start-element-vars*- rule-props-to-index)
                                      (cond-> atts (start-element-atts*- atts)))
                                  (-> state
                                      (update-in [:stk] conj 
                                                 {:ruleno ruleno, 
                                                  :end-element end-el-f})
                                      (update-in [:var-idx] conj
                                                 (or ext-varidx {}))
                                      (cond-> atts 
                                              (start-element-atts*- atts)))))
                              :prune
                              (do
                                (start-element-pruning* state)))))))) 
             rules)))))

(defn analyze-rules-*-reverse-path-index
  [cfg]
  (assoc cfg :rev-path-to-ruleno
         (reduce (fn [m {:keys [path ruleno] :as rule}]
                   (when-not path 
                     (throw (RuntimeException. (str "Rule has no path: " rule))))
                   (assoc-in m 
                             (-> path
                                 (string/split #"/")
                                 (reverse)
                                 (concat [:end]))
                             ruleno))
                 {}
                 (:rules cfg))))

(defn analyze-rules
  [rules symbols]
  (->> {:rules rules}
       (analyze-rules-*-number)
       (analyze-rules-*-compose-end-element symbols)
       (analyze-rules-*-compose-start-element)
       (analyze-rules-*-reverse-path-index)))

(defn ruleno-from-index
  ;; problem : path a/b and path a-only conflict.
  ;; solution: regard them as paths a/:end and a/b/:end.
  "Given reverse-path index and a reverse path, trace the path into the
  index far enough to get an integer. Nil if none."
  [idx path]
  (when-let [t (first path)]
    (if-let [next-idx (get idx t)]
      (if-let [deeper-answer (ruleno-from-index next-idx (rest path))]
        deeper-answer
        (:end next-idx)))))

(defn start-element-dflts [state]
  (-> state 
      (update-in [:stk] conj nil)
      (start-element-novars*)))

(defn start-element-rule
  "Identifies rule suitable for state's :rtags. Pushes stack frame."
  [{:keys [rev-path-to-ruleno rtags] :as state}]
  (if-let [ruleno (ruleno-from-index rev-path-to-ruleno rtags)]
    (let [start-el-f (get-in state [:rules ruleno :start-element])]
      (start-el-f state))
    (start-element-dflts state)))

(defn start-element [state]
  (let [^XMLStreamReader xsr (:xsr state)] 
    (-> state 
        (update-in [:rtags] conj (.getLocalName xsr))
        (start-element-rule))))

(defn on-chars [state, ^XMLStreamReader xsr]
  (when-let [^StringBuilder buf (-> state :stk peek :text-buf)]
    (.append buf (.getText xsr)))
  state)

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
      (assoc :var-idx []) ;; stack of hash of var key to stk index
      (assoc :xsr xsr)))

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
