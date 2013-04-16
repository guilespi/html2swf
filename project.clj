(defproject html2swf "0.2.0"
  :description "Testlet converter service"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main html2swf.main
  :resource-paths ["resources"]
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [clj-css "0.1.1"]
                 [enlive "1.1.2"]
                 [hiccup "1.0.3"]
                 [midje "1.5.1"]
                 [org.clojure/tools.cli "0.2.2"]])
