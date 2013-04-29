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
                          #"nth-of-type\(([0-9]+|even|odd)\)" 
                          #(str "nth-of-type-" (second %1))))

(defn normalize-path
  "Given a path with relative references collapses it
   So /this/is/a/../path returns /this/is/path"
  [path]
  (if-let [[_ separator] (re-find #".*([/\\]).+$" path)]
    ;;windows paths are converted to forward slash ones /
    (let [path (clojure.string/replace path #"\\" "/")
          dirs (clojure.string/split path (re-pattern separator))
          collapsed (reduce (fn [list dir] (case dir 
                                             "." list 
                                             ".." (vec (butlast list))
                                             (conj list dir))) [] dirs)]
      (clojure.string/join separator collapsed))
    path))

(defn fix-url-paths
  "Fixes the absolute url paths to absolutes. This avoids having to move each
   css context with the styles in order to know what '../../' means"
  [content path]
  (if (and (string? path) (re-find  #"url\(\"?([^\"\)]+)\"?\)" content))
    (if-let [[_ base-path css-file] (re-find #"(.+[/\\])([^/\\]+$)" path)]
      (clojure.string/replace content 
                              #"url\(\"?([^\"\)]+)\"?\)"
                              #(format "url(\"%s\")" (normalize-path (str base-path (get %1 1)))))
      content)
    content))

(defn- parse-stylesheets
  [sheets]
  (let [contents (map (fn [path] ( -> (slurp path) 
                                      debomify 
                                      dos2unix 
                                      hack-nth-of-type 
                                      (fix-url-paths path))) sheets)]
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