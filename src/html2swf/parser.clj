(ns html2swf.parser
  (:require [clojure.java.io]
            [net.cgrand.enlive-html :as html]
            [clj-css.core :as css])
  (:use [html2swf.utils]))

(def ^:dynamic *default-stylesheet* "resources/css/default.css")

(defn- stylesheets
  [h directory]
  (cons *default-stylesheet*
        (map (comp (partial str directory) :href :attrs) 
             (html/select h [[:link (html/attr|= :rel "stylesheet")]]))))

(defn- parse-stylesheets
  [sheets]
  (let [contents (map #(dos2unix (debomify (slurp %))) sheets)]
    (filter seq 
            (apply concat
                   (map #(css/parse-css %) contents)))))

(defn read-html-files
  "Reads a directory from disk recursively and parses all HTML files.

  Returns a map with the following structure:
      {filepath {:content
                 :relative-path}
  "
  [directory]
  (let [files (file-seq (clojure.java.io/file directory))
        html-files (filter #(re-matches #".+\.html$" (.getName %)) files)]
    (reduce
      (fn [h f] 
        (let [full-path (.getPath f)
              relative-path (clojure.string/replace full-path directory "")]
          (assoc h full-path {:html (html/html-resource (clojure.java.io/file full-path))
                              :relative-path relative-path}))) {} html-files)))

(defn styles-for-page
  [html-content base-directory]
  (let [sheets (stylesheets html-content base-directory)]
    (parse-stylesheets sheets)))