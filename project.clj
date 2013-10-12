(defproject com.hoofdust/xml-skim "0.1.0-SNAPSHOT"
  :description "Skims data structures from XML using streaming APIs"
  :url "http://hoofdust.com/"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :profiles {:dev {:source-paths ["dev"]
                   :resource-paths ["dev-resources"]
                   :global-vars {*warn-on-reflection* true
                                 *assert* true}}})
