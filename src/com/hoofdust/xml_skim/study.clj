(ns com.hoofdust.xml-skim.study
  "Creates start- and end-element handlers based on configuration and
  document structure as discovered"
  (:import [javax.xml.stream 
            XMLStreamConstants XMLStreamReader])
  (:require [clojure.set :as set] 
            [clojure.string :as string]
            [clojure.core.match :as match]
            [com.hoofdust.xml-skim.cfg :as cfg]
            [com.hoofdust.xml-skim.stax :as stax]))

(defn assign-atts
  "Given a product stack, an XMLStreamReader that is positioned at
  a start-element that might have attributes, and a map of
  attribute-name to fn of (product stack and new value), thread the
  product stack through the fns for the element's actual
  attribute values."
  [stk sink ^XMLStreamReader xsr]
  (loop [stk stk, i (dec (.getAttributeCount xsr))]
    (if (neg? i)
      stk
      (recur 
       (or (some-> 
            (.getAttributeLocalName xsr i)
            (sink)
            (as-> X (X stk (.getAttributeValue xsr i))))
           stk)
       (dec i)))))

(defn end-element-f
  "Function to handle an end-element StAX event in the context of path
  parameters pp, or nil if no end-element handler is necessary"
  [symbols pp]
  (let [criteria (let [rule (get pp :rule)]
                   [(when (get rule :create)
                      :create)
                    (when-let [f (some-> (get-in rule [:create :complete-by])
                                         (symbols))]
                      [f])
                    (when-let [s (get-in pp [:sink :ob])]
                      [s])
                    (when-let [e (get-in rule [:create :eject])]
                      [e])])]
    (match/match 
     criteria

     [nil _ _ _] ;; no create - nothing to pop, complete, assign, or eject
     nil

     [:create _ nil nil] ;; create, pointless with neither assign nor eject
     (fn [stk]
       [(pop stk)])
     
     [:create nil nil [e]] ;; create and eject
     (fn [stk]
       (let [ob (peek stk)]
         [(pop stk)
          ob]))
     
     [:create nil [s] nil] ;; create and assign
     (fn [stk]
       (let [ob (peek stk)]
         [(-> stk
              (pop)
              (cond-> ob (s ob)))]))

     [:create nil [s] [e]] ;; create, assign, and eject
     (fn [stk]
       (let [ob (peek stk)]
         [(-> stk
              (pop)
              (cond-> ob (s ob)))
          ob]))
     
     [:create [f] nil [e]] ;; create, finish, eject
     (fn [stk]
       (let [ob (some-> (peek stk)
                        (f))]
         [(pop stk)
          ob]))
     
     [:create [f] [s] nil] ;; create, finish, assign
     (fn [stk]
       (let [ob (some-> (peek stk)
                          (f))]
         [(-> stk
              (pop)
              (cond-> ob (s ob)))]))
     
     [:create [f] [s] [e]] ;; create, finish, assign, eject
     (fn [stk]
       (let [ob (some-> (peek stk)
                          (f))]
         [(-> stk
              (pop)
              (cond-> ob (-> (s ob))))
          ob])))))

(defn start-element-f
  "Two-item vector - first, a boolean indicating whether the
  start-element handler may change the element nesting level (It does
  not change the nesting level if it consumes not only the
  start-element event, but also any text and the ensuing end-element
  event); and second, a start-element handler function."
  [pp]
  (let [rule   (get pp :rule)
        atts   (get rule :atts)
        sink   (get pp :sink)
        text-sink (get sink :text)
        
        ;;criteria [create? atts? content?]
        criteria [(when (get rule :create)     :create)
                  (when (get rule :atts)       :atts)
                  (cond text-sink              :text-keep
                        (get rule :text-value) :text-drop
                        (get rule :prune)      :prune
                        :else              :children )]]
    (match/match
        criteria
      
      [:create :atts :text-keep]
      [true (fn [stk ^XMLStreamReader xsr]
               (-> stk
                   (conj nil)
                   (assign-atts sink xsr)
                   (text-sink (.getElementText xsr))))]
      
      [:create :atts :text-drop]
      [true (fn [stk ^XMLStreamReader xsr]
              (.getElementText xsr) ;; Advances XSR to End.
              (-> stk
                  (conj nil)
                  (assign-atts sink xsr)))]
      
      [:create nil :text-keep]
      [true (fn [stk ^XMLStreamReader xsr]
               (-> stk
                   (conj nil)
                   (text-sink (.getElementText xsr))))]
      
      [ _ nil :text-drop]
      [false (fn [stk ^XMLStreamReader xsr]
              (.getElementText xsr) ;; Advances XSR to End.
              (when (stax/advance-xsr xsr)
                stk))]
      
      [nil :atts :text-keep]
      [true (fn [stk ^XMLStreamReader xsr]
              (-> stk
                  (assign-atts sink xsr)
                  (text-sink (.getElementText xsr))))]
      
      [nil nil :text-keep]
      [true (fn [stk ^XMLStreamReader xsr]
              (text-sink stk (.getElementText xsr)))]
      
      [nil :atts :text-drop]
      [true (fn [stk ^XMLStreamReader xsr]
              (.getElementText xsr) ;; Advances XSR to End.
              (assign-atts stk sink xsr))]
      
      [nil nil :prune]
      [false (fn [stk ^XMLStreamReader xsr]
               (stax/burn-stax xsr)
               (when (stax/advance-xsr xsr)
                 stk))]
      
      [:create nil :children]
      [true (fn [stk ^XMLStreamReader xsr]
              (when (stax/advance-xsr xsr)
                (conj stk nil)))]
      
      [:create :atts :children]
      [true (fn [stk ^XMLStreamReader xsr]
              (let [stk' (-> stk
                             (conj nil)
                             (assign-atts sink xsr))]
                (when (stax/advance-xsr xsr)
                  stk')))]
      
      [nil :atts :children]
      [true (fn [stk ^XMLStreamReader xsr]
              (let [stk' (assign-atts stk sink xsr)]
                (when (stax/advance-xsr xsr)
                  stk')))]
      
      [nil nil :children]
      [true (fn [stk ^XMLStreamReader xsr]
              (when (stax/advance-xsr xsr)
                stk))]

      :else
      (throw (RuntimeException. (str "What to do with " criteria))))))

(defn path-disposal 
  "Function of a reverse path that yields a map containing:

  * :rpath - the reverse path.

  * :rulestk - stack (vector) of non-nil rules for this reverse-path
    and its ancestors'.

  * :rule - the rule that applies to the rpath. May be nil, but
    otherwise same as peek of rulestk.

  * :var-idx - property-key to rulestk-index map, considering the
    above rule's props, and all ancestor elements'.

  * :sink - map of attribute name, or :text for character content, or
    :ob for the map produced by a :create configuration rule, to a fn
    of product-stack and a value, that either assigns the value to the
    property or conj's it if the property is a multi, and returns
    revised product stack.

  * :start-element - function of product-stack and XMLStreamReader
    that adjusts the product stack in response to a start-element
    event and advances the XMLStreamReader

  * :end-element - function of product stack that adjusts the product
    stack in response to a StAX end-element event.

  The function is auto-memoizing."
  [rules symbols]
  (let [rev-idx        (cfg/reverse-path-index rules) 
        assign-targets (cfg/prop-targets rules)
        memo           (atom {})] 
    (letfn [(updater [stk var-idx prop]
              (when-let [frame-no (var-idx prop)]
                ;; Faster exactly-2-step versions of update-in and assoc-in:
                (if (-> stk (get frame-no) :create :props prop :multi)
                  (fn u9 [stk v] 
                    (assoc stk frame-no
                           (let [q (get stk frame-no)] 
                             (assoc q prop (conj (or (get q prop) []) v)))))
                  (fn a1 [stk v]
                    (assoc stk frame-no 
                           (assoc (get stk frame-no) prop v))))))
            (new-disposal [rpath]
              (->
               (if (seq rpath)
                 (let [parent        (disposal (drop 1 rpath))]
                   (if-let [ruleno   (cfg/ruleno-from-index rev-idx rpath)]
                     (let [p'rulestk (get parent :rulestk)
                           frameno   (count p'rulestk)
                           rule      (get rules ruleno)
                           rulestk   (conj p'rulestk rule)
                           props     (filter 
                                      assign-targets 
                                      (-> rule :create :props keys))
                           ob-prop   (get-in rule [:create :assign])
                          
                           t-prop    (-> rule :text-value :assign)
                          
                           p'var-idx (get parent :var-idx)
                           var-idx   (merge p'var-idx
                                            (zipmap props (repeat frameno)))
                          
                           sink (->> (apply concat
                                            (when ob-prop 
                                              [[:ob p'var-idx ob-prop]])
                                            (when t-prop 
                                              [[:text var-idx t-prop]])
                                            (for [[att {prop :assign}] 
                                                  (get rule :atts)
                                                  :when prop] 
                                              [[att var-idx prop]]))
                                     (map (fn [[k v p]]
                                            (when-let 
                                                [u (updater rulestk v p)]
                                              {k u})))
                                     (apply merge {})) ]
                       ;; There is a rule number:
                       {:rule rule,
                        :rpath rpath,
                        :rulestk rulestk, 
                        :var-idx var-idx, 
                        :sink sink})
                     ;; No rule number, but rev.path is non-empty:
                     (assoc parent 
                       :rule nil
                       :rpath rpath)))
                 ;; Rev.path is empty:
                 {:rule nil,
                  :rulestk [], 
                  :var-idx {}, 
                  :sink {}
                  :rpath rpath})
               ;; Compose start- and end-element handlers based on the above:
               (as-> x (merge x
                              (zipmap [:depth-change :start-element] 
                                      (start-element-f x))))
               (as-> x (assoc x :end-element (end-element-f symbols x)))))
            ;; Main function:
            (disposal [rpath]
              (if-let [ret (@memo rpath)]
                ret
                (let [v (new-disposal rpath)]
                  (swap! memo assoc rpath v)
                  v)))]
      disposal)))
