(defproject com.hoofdust/xml-skim "0.3"
  :description "Skims data structures from XML using JAXP streaming API"
  :url "http://hoofdust.com/"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.match "0.2.0"]]
  :profiles {:dev {:source-paths ["dev"]
                   :resource-paths ["dev-resources"]
                   :global-vars {*warn-on-reflection* true
                                 *assert* true}}})
