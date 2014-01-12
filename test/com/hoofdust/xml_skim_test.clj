(ns com.hoofdust.xml-skim-test
  (:require [clojure.edn :as edn] 
            [clojure.java.io :as io] 
            [clojure.test :refer :all]
            [com.hoofdust.xml-skim :refer :all]))

(defn xml-stream-reader 
  "XMLStreamReader open on a URI (string), or a Reader"
  ^javax.xml.stream.XMLStreamReader [well]
  (.. javax.xml.stream.XMLInputFactory
      (newFactory)
      (createXMLStreamReader 
       (javax.xml.transform.stream.StreamSource. well))))

(deftest no-rules
  (with-open [xsr (xml-stream-reader (str (io/resource "record.xml")))]
    (testing "no rules"
      (is (= (vec (pull-seq [] {} xsr)) 
             [])))))

(deftest empty-doc
  (let [rules    (edn/read-string (slurp (io/resource "record_rules.edn")))]
    (with-open [xsr (xml-stream-reader (java.io.StringReader. ""))]
      (testing "record rules"
        (is (thrown? javax.xml.stream.XMLStreamException
                     (vec (pull-seq 
                           rules 
                           {'complete-part
                            (fn [part] 
                              (update-in part [:qty] #(Long/parseLong %)))}
                           xsr))))))))

(deftest record-rules
  (let [rules    (edn/read-string (slurp (io/resource "record_rules.edn"))) 
        expected (edn/read-string (slurp (io/resource "record.edn"))) ]
    (with-open [xsr (xml-stream-reader (str (io/resource "record.xml")))]
      (testing "record rules"
        (is (= (vec (pull-seq rules 
                              ;; test returning a fixed-up object.
                              ;; test returning nil instead of object.
                              {'complete-part
                                (fn [part] 
                                  (let [qty (Long/parseLong (:qty part))] 
                                    (when ( < 0 qty ) 
                                      (assoc-in part [:qty] qty))))}
                              xsr)) 
               [expected]))))))

