(ns html2swf.compiler
  (:require [html2swf.translator :as translator]
            [html2swf.parser :as parser])
  (:use [clojure.java.shell :only [sh]]
        [html2swf.utils]))

(def ^:dynamic *compiler* "/Users/guilespi/Downloads/flex_sdk_4.6/bin/mxmlc")

(defn compile-source
  "From a proper mxml string create a filename.swf file"
  [filename content]
  (println (format "Compiling %s.mxml ..." filename))
  (let [source-file (str filename ".mxml")
        mxml (str "<?xml version=\"1.0\" encoding=\"utf-8\"?>" content)
        compiler (or (System/getenv "MXML_COMPILER") *compiler*)]
    (spit source-file mxml :append false)
    (let [result (sh compiler "-debug=false" "-optimize=true" "-swf-version=10" source-file)
          exit (:exit result)]
      (= 0 exit))))

(defn compile-file
  "Build one html file to swf using base-directory 
   as root for css and image search"
  [htmlfile base-directory width height]
  (let [filename (first htmlfile)
        _ (println "Compile file " filename)
        html-content (:html (second htmlfile))
        styles (parser/styles-for-page html-content base-directory)
        object-content (translator/translate-page html-content styles width height)
        [_ component-name] (re-find #"[/\\]([^/\\]+)\.html$" filename)
        escaped-name (clojure.string/replace component-name #"[-\.]" "")]
    (compile-source (str base-directory escaped-name) object-content)))

(defn build-directory
  "Convert a complete directory of html files to swf ones"
  [base-directory width height]
  (println "Building directory" base-directory)
  (let [files (parser/read-html-files base-directory)]
    (doall (map #(compile-file % base-directory width height) files))))

(defn build-file
  [filepath width height]
  (println "Building file" filepath)
  (when-let [html-file (parser/read-html-file filepath)]
    (let [[_ base-directory] (re-find #"(.+/)[^/]+$" filepath)]
      (compile-file [filepath html-file] base-directory width height))))