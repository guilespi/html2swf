(ns html2swf.parser
  (:require [clojure.java.io]
            [net.cgrand.enlive-html :as html]
            [clj-css.core :as css])
  (:use [html2swf.utils]))

(def ^:dynamic *default-stylesheet* "css/default.css")

(defn- stylesheets
  [h directory]
  (cons (clojure.java.io/resource *default-stylesheet*)
        (map (comp (partial str directory) :href :attrs)
             (html/select h [[:link (html/attr|= :rel "stylesheet")]]))))


(defn- hack-nth-of-type
  ;;css parser does not support css3 selector nth-of-type as is, tweak it a little bit 
  [content]
  (clojure.string/replace content 
                          #"nth-of-type\(([0-9]+)\)" 
                          #(str "nth-of-type-" (second %1))))

(defn- parse-stylesheets
  [sheets]
  (let [contents (map #(-> (slurp %) debomify dos2unix hack-nth-of-type) sheets)]
    (filter seq (apply concat
                       (map #(css/parse-css %) contents)))))

(defn- is-html?
  [file]
  (re-matches #".+\.html$" (.getName file)))

(defn read-html-files
  "Reads a directory from disk recursively and parses all HTML files.

  Returns a map with the following structure:
      {filepath {:content
                 :relative-path}
  "
  [directory]
  (let [files (file-seq (clojure.java.io/file directory))
        html-files (filter is-html? files)]
    (reduce
      (fn [h f] 
        (let [full-path (.getPath f)
              relative-path (clojure.string/replace full-path directory "")]
          (assoc h full-path {:html (html/html-resource (clojure.java.io/file full-path))
                              :relative-path relative-path}))) {} html-files)))

(defn read-html-file
  [filepath]
  (let [file (clojure.java.io/file filepath)]
    (when (is-html? file)
      {:html (html/html-resource file)})))

(defn styles-for-page
  [html-content base-directory]
  (let [sheets (stylesheets html-content base-directory)]
    (parse-stylesheets sheets)))