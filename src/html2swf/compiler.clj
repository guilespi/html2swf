(ns html2swf.compiler
  (:require [html2swf.translator :as translator]
            [html2swf.parser :as parser])
  (:use [clojure.java.shell :only [sh]]
        [html2swf.utils]))

(def ^:dynamic *compiler* "/Users/guilespi/Downloads/flex_sdk_4.6/bin/mxmlc")

(defn compile-source
  "From a proper mxml string create a filename.swf file"
  [filename content]
  (println (format "Compiling %s..." filename))
  (let [source-file (str filename ".mxml")
        mxml (str "<?xml version=\"1.0\" encoding=\"utf-8\"?>" content)]
    (spit source-file mxml :append false)
    (let [result (sh *compiler* "-debug=false" "-swf-version=10" source-file)
          exit (:exit result)]
      (= 0 exit))))

(defn build-file
  "Build one html file to swf using base-directory 
   as root for css and image search"
  [htmlfile base-directory]
  (let [filename (first htmlfile)
        _ (println "Building file " filename)
        html-content (:html (second htmlfile))
        styles (parser/styles-for-page html-content base-directory)
        object-content (translator/translate-page html-content styles)
        base-name (clojure.string/replace filename #".html" "")]
    (compile-source base-name object-content)))

(defn build-directory
  "Convert a complete directory of html files to swf ones"
  [base-directory]
  (println "Building directory " base-directory)
  (let [files (parser/read-html-files base-directory)]
    (doall (map #(build-file % base-directory) files))))
