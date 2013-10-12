(ns com.hoofdust.xml-skim
  "Reads XML, described by rules, into structures"
  (:import [javax.xml.stream XMLStreamConstants XMLStreamReader])
  (:require [clojure.set :as set] 
            [clojure.string :as string]))

(defn prop-targets-nopath 
  "Set of :assign targets within a :text-value, :create, or attribute"
  [rules]
  (set (remove nil?
               (for [rule rules, 
                     kk `[[:create] 
                          [:text-value] 
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

(declare pull-parse-event)
(declare pull-parse-event-chars)
(declare pull-parse-event-pruning)

(defn start-element-pruning*
  [state]
  (update-in state [:event-handler] conj pull-parse-event-pruning))

(defn start-element-rule* 
  [ruleno end-el-f state]
  (update-in state [:stk] conj {:ruleno ruleno, :end-element end-el-f}))

(defn start-element-textbuf*
  [{:keys [stk] :as state}]
  (-> state 
      (assoc-in [:stk (dec (count stk)) :text-buf] (StringBuilder.))
      (update-in [:event-handler] conj pull-parse-event-chars)))

(defn start-element-notextbuf*
  [state]
  (update-in state [:event-handler] conj pull-parse-event))

(defn start-element-novars*
  [state]
  (let [var-frames (or (peek (:var-idx state)) {})]
    (update-in state [:var-idx] conj var-frames)))

(defn start-element-vars*
  [rule-props-to-index state]
  (let [stk (:stk state)
        last-frame-no (dec (count stk))
        var-frames (peek (:var-idx state))]
    (update-in state [:var-idx] conj 
               (merge var-frames 
                      (zipmap rule-props-to-index (repeat last-frame-no))))))

(defn start-element-atts*
  [att-defs {:keys [rules stk] :as state}]
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

(defn start-element-nexthdlr*
  [hdlr state]
  (update-in state [:event-handler] conj hdlr))

(defn end-element-assign-text* 
  [t-p, {:keys [stk rules var-idx] :as state}]
  (let [{:keys [text-buf] :as frame} (peek stk)
        [t-uks t-uf] (assign-vec rules stk (peek var-idx) t-p)]
    (cond-> state t-uks (update-in t-uks t-uf (str text-buf)))))

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

(defn end-element-nexthdlr*
  [state]
  (update-in state [:event-handler] pop))

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
                    (when-let [t-p (-> rule :text-value :assign)]
                      [(partial end-element-assign-text* t-p)])

                    [end-element-varidx]

                    (when-let [complete-f 
                               (some-> (get-in rule [:create :complete-by])
                                       (symbols))]
                      [(partial end-element-complete* complete-f)])

                    (when-let [o-p (get-in rule [:create :assign])]
                      [(partial end-element-assign-ob* o-p)])

                    (when-let [eject-how (get-in rule [:create :eject])]
                      [(partial end-element-eject-ob* eject-how)])

                    [end-element-nexthdlr*] 
                    ]
                   
                   (apply concat)
                   (reverse) ;; because comp runs fns right-to-left
                   (apply comp)
                   (assoc rule :end-element)))) 
          rules))))

(defn- analyze-rules-*-compose-start-element
  "Composes a :start-element handler for each rule"
  [cfg]
  (let [assign-targets (prop-targets-nopath (:rules cfg))] 
    (update-in 
     cfg [:rules]
     (fn [rules]
       (mapv (fn [rule] 
              (->> (if (:prune rule)
                     [[start-element-pruning*]]
                     [
                      ;; Steps for a start-element handler:
                      [(partial start-element-rule* 
                                (:ruleno rule)
                                (:end-element rule))]
                      
                      (if (:text-value rule)
                        [start-element-textbuf*]
                        [start-element-notextbuf*])
                      
                      (if-let 
                          [rule-props-to-index
                           (when-let [props (-> rule :create :props keys set)]
                             (seq (set/intersection assign-targets props)))]
                        [(partial start-element-vars* rule-props-to-index)]
                        [start-element-novars*])
                      
                      (when-let [rule-atts (:atts rule)]
                        [(partial start-element-atts* rule-atts)])
                      ])
                   (apply concat)
                   (reverse) ;; because comp runs fns right-to-left
                   (apply comp)
                   (assoc rule :start-element))) 
            rules)))))

(defn analyze-rules-*-reverse-path-index
  [cfg]
  (assoc cfg :rev-path-to-ruleno
         (reduce (fn [m {:keys [path ruleno] :as rule}]
                   (assoc-in m 
                             (-> path
                                 (string/split #"/")
                                 (reverse))
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
  "Given reverse-path index and a reverse path, trace the path into the
  index far enough to get an integer. Nil if none."
  [idx path]
  (when-let [t (first path)]
    (when-let [next-idx (get idx t)]
      (if (number? next-idx)
        next-idx
        (recur next-idx (rest path))))))

(defn start-element-rtags 
  "Returns state, with XSR's element local-name pushed onto the :rtags stack."
  [state]
  (let [^XMLStreamReader xsr (:xsr state)] 
    (update-in state [:rtags] conj (.getLocalName xsr))))

(defn start-element-dflts [state]
  (-> state 
      (update-in [:stk] conj nil)
      (start-element-novars*)
      (start-element-notextbuf*)))

(defn start-element-rule
  "Identifies rule suitable for state's :rtags. Pushes stack frame."
  [{:keys [rev-path-to-ruleno rtags] :as state}]
  (if-let [ruleno (ruleno-from-index rev-path-to-ruleno rtags)]
    (let [start-el-f (get-in state [:rules ruleno :start-element])]
      (start-el-f state))
    (start-element-dflts state)))

(defn start-element [state]
  (-> state 
      (start-element-rtags)
      (start-element-rule)))

(def end-element-dflts
  (comp end-element-varidx end-element-nexthdlr*))

(defn end-element [state]
  (let [end-el-f (or (-> state :stk peek :end-element)
                     end-element-dflts)]
    (-> state
        (end-el-f)
        (update-in [:stk] pop)
        (update-in [:rtags] pop))))

(defn on-chars [state, ^XMLStreamReader xsr]
  (when-let [^StringBuilder buf (-> state :stk peek :text-buf)]
    (.append buf (.getText xsr)))
  state)

(defn pull-parse-event
  "Computes new state, based on old state and event. Returns new state,
  or nil if the event indicates that the document is over."
  [state]
  (let [^XMLStreamReader xsr (:xsr state) 
        e (.getEventType xsr)]
    (cond
     (= e XMLStreamConstants/END_DOCUMENT)
     nil
     
     (= e XMLStreamConstants/START_ELEMENT)
     (start-element state)
     
     (= e XMLStreamConstants/END_ELEMENT)
     (end-element state)
     
     ;; else:
     ;; XMLStreamConstants/CHARACTERS
     ;; XMLStreamConstants/START_DOCUMENT
     ;; XMLStreamConstants/COMMENT
     ;; XMLStreamConstants/PROCESSING_INSTRUCTION 
     ;; XMLStreamConstants/SPACE
     :else
     state)))

(defn pull-parse-event-chars
  "Computes new state, based on old state and event. Returns new state,
  or nil if the event indicates that the document is over."
  [state]
  (let [^XMLStreamReader xsr (:xsr state) 
        e (.getEventType xsr)]
    (cond
     (= e XMLStreamConstants/END_DOCUMENT)
     nil
     
     (= e XMLStreamConstants/START_ELEMENT)
     (start-element state)
     
     (= e XMLStreamConstants/END_ELEMENT)
     (end-element state)
     
     (= e XMLStreamConstants/CHARACTERS)
     (on-chars state xsr)
     
     ;; else:
     ;; XMLStreamConstants/START_DOCUMENT
     ;; XMLStreamConstants/COMMENT
     ;; XMLStreamConstants/PROCESSING_INSTRUCTION 
     ;; XMLStreamConstants/SPACE
     :else
     state)))

(defn pull-parse-event-pruning
  [state]
  (let [^XMLStreamReader xsr (:xsr state) 
        e (.getEventType xsr)]
    (cond
     (= e XMLStreamConstants/END_DOCUMENT)
     nil
     
     (= e XMLStreamConstants/START_ELEMENT)
     (update-in state [:event-handler] conj pull-parse-event-pruning)
     
     (= e XMLStreamConstants/END_ELEMENT)
     (update-in state [:event-handler] pop)
     
     ;; else:
     ;; XMLStreamConstants/START_DOCUMENT
     ;; XMLStreamConstants/CHARACTERS
     ;; XMLStreamConstants/COMMENT
     ;; XMLStreamConstants/PROCESSING_INSTRUCTION 
     ;; XMLStreamConstants/SPACE
     :else
     state)))

(defn pull-parse-event-advance
  "Computes new state, based on old state and event, ejects the
  object that the event completes (if any), and advances xsr to the
  next event. Returns new state, or nil if document is over."
  [state]
  (let [^XMLStreamReader xsr (:xsr state) 
        handler (peek (:event-handler state))
        state' (handler state)]
    (when state'
      (assert (identical? xsr (:xsr state')))
      (when (.hasNext xsr)
        (.next xsr) 
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
      (assoc :xsr xsr)
      (assoc :event-handler (list pull-parse-event))))

(defn pull-object
  "Interprets XML stream until an object is ready to eject. Returns a
  vector containing the object and the state with which next to call
  pull-object. Returns nil, instead of the vector, when the stream is
  over."
  [state]
  (loop [state state]
    (when-let [state' (pull-parse-event-advance state)]
      (if-let [eject (:eject state')] 
        [eject (dissoc state' :eject)]
        (recur state')))))

(defn pull-seq
  "Lazy sequence of objects pulled from the XML stream"
  [rules symbols ^XMLStreamReader xsr]
  (letfn [(pull-seq* [state]
            (lazy-seq 
             (when-let [[ob state'] (pull-object state)]
               (cons ob (pull-seq* state')))))]
    (pull-seq* (start-pull rules symbols xsr))))
