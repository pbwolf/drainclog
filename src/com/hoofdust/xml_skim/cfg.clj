(ns com.hoofdust.xml-skim.cfg
  "Interprets configuration structure"
  (:require [clojure.string :as string]))

(defn prop-targets 
  "Scans rules and extracts a set of \":assign\" -targeted attribute keywords"
  [rules]
  (set (remove nil?
               (for [rule rules, 
                     kk `[[:create] 
                          [:text-value] 
                          ~@(for [a (-> rule :atts keys)] [:atts a])]]
                 (get-in rule `[~@kk :assign])))))

(defn reverse-path-index
  "Structure for ruleno-from-index to use. It is a nested map where the
  first lookup is of the leaf element tag name (from the rule's :path
  member), the second lookup is of the leaf's immediate parent (if the
  rule specifies it), etc.  Finally (since one rule's path tail may be
  an extension of another's) the last lookup is of the keyword :end
  and yields the index number of the rule in the rule vector."
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
