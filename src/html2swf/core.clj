(ns html2swf.core
  (:require [clojure.java.io]
            [net.cgrand.enlive-html :as html]
            [hiccup.core :as hiccup]
            [clj-css.core :as css])
  (:use [clojure.java.shell :only [sh]]))

(defn debomify
  [^String line]
  (let [bom "\uFEFF"]
    (if (.startsWith line bom)
      (.substring line 1)
      line)))

(defn dos2unix
  [^String content]
  (clojure.string/replace content #"\r" ""))

(defn inline-trim
  [^String string]
  (clojure.string/replace string #"\s\s+" " "))

(def ^:dynamic *default-stylesheet* "resources/css/default.css")
(defn stylesheets
  [h directory]
  (cons *default-stylesheet*
        (map (comp (partial str directory) :href :attrs) 
             (html/select h [[:link (html/attr|= :rel "stylesheet")]]))))

(defn parse-stylesheets
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

(defn children
  "Shortcut for using Enlive to get all elements beneath an element"
  [node]
  (filter map? (:content node)))

(defn parse-selectors
  "For a given rule of the form [:*#navbar :ul :*#navbar2 :li{:other 23, :width 10}]
   Returns a sequence of the different selector paths ([#navbar ul] [#navbar2 li])"
  [style]
  (let [selectors (filter #(not (map? %)) style)]
    (reduce (fn [v sel]
              (let [str-sel (name sel)
                    last-index (dec (count v))] 
                (if (or (.startsWith str-sel "*") (= last-index -1))
                  (conj v [(clojure.string/replace str-sel #"^\*" "")])
                  (assoc v last-index (conj (get v last-index) str-sel))))) [] selectors)))

(defn has-class?
  [node class]
  (when-let [classes (:class (:attrs node))]
    (>= (.indexOf (clojure.string/split classes #" ") class) 0)))

(defmulti partial-condition-match? (fn [condition node]
                                     (cond 
                                        (re-find #"^#.+" condition) ::selector-id
                                        (re-find #"^[-_a-zA-Z0-9]+\.[-_a-zA-Z0-9]+$" condition) ::selector-classed-tag
                                        (re-find #"^\.[-_a-zA-Z0-9]+$" condition) ::selector-class
                                        (re-find #"^[-_a-zA-Z0-9]+$" condition) ::selector-tag)))

(defmethod partial-condition-match? ::selector-id
  [condition node]
  (let [id (:id (:attrs node))]
    (= condition (str "#" id))))

(defmethod partial-condition-match? ::selector-class
  [condition node]
  (when-let [[_ class] (re-find #"^\.([-_a-zA-Z0-9]+)$" condition)] 
    (has-class? node class)))

(defmethod partial-condition-match? ::selector-tag
  [condition node]
  (= condition (name (:tag node))))

(defmethod partial-condition-match? ::selector-classed-tag
  [condition node]
  (when-let [[_ tag class] (re-find #"^([-_a-zA-Z0-9]+)\.([-_a-zA-Z0-9]+)$" condition)] 
    (and (= tag (name (:tag node)))
         (has-class? node class))))

(defmethod partial-condition-match? :default
  [condition node]
  (println (format "Unable to identify css selector '%s'" condition)))

(defn selector-match?
  "Returns true if the last element of the selector matches the specified node
   That is the selector [body #id1 ul] matches for the node :ul"
  [node selector]
  (cond
   (nil? node) false
   (nil? selector) true
   (not (seq selector)) true
   :else  (partial-condition-match? (last selector) node)))

(defn selector-applies?
  "Returns true if the selector applies for the current node"
  [node ancestry selector]
  (loop [n node parents ancestry s selector]
    (comment (println "recurrring with n " n " parents " parents " selector " s))
    ;;current node matches for last selector tag
    (when (selector-match? n s)
      ;;if no more selectors found, this is a WIN
      (if-let [selector (butlast s)]
        ;;drop parents until found one matching the next selector
        ;;eg. "body div ul.menu" may have bogus nodes in the hierarchy
        (let [ancestry (drop-while #(not (selector-match? % selector)) parents)] 
          (recur (first ancestry) (rest ancestry) selector))
        true))))

(defn style-applies?
  "Returns true if the style matches for the current node"
  [node ancestry style]
  (let [selectors (parse-selectors style)]
    (seq (filter #(selector-applies? node ancestry %) selectors))))

(defn get-style-attributes
  "Each style rule has only one hashmap of attributes at the end
   This function returns the only hashmap of the style sequence"
  [style]
  (first (filter map? style)))

(def ^:dynamic *inherited-css-attributes* [:font])

(defn get-inherited-style-attrs
  "Retrieve for a style only the attributes propagating to the childs"
  [style]
  (let [attrs (get-style-attributes style)]
    (select-keys attrs *inherited-css-attributes*)))

(defn styles-for-node
  "Creates a hashmap of style attributes for a given node
   assuming styles have all the stylesheets for page combined"
  [node ancestry styles]
  (reduce #(if (style-applies? node ancestry %2)
             (merge %1 (get-style-attributes %2))
             %1) {} styles))

(defn extract-image
  "Read other attributes of the image such as location (right bottom etc"
  [attrs]
  (when-let [background-image (:background-image attrs)]
    ;;FIXME: this extraction is broken only works if images and CSS are in subdirectories
    ;;of the main path, probably to happen, but broken anyway
    {:path (second (re-find #"url\(\.\./([^)]+)" background-image))
     :position (if (and (:background-position attrs)
                        (re-find #"left" (:background-position attrs)))
                 :left
                 :right)}))

(defn rgb-int-from-components
  "Convert a vector of the 3 rgb integer values into a color given in
  numeric rgb format"
  [r g b]
  (bit-or (bit-shift-left (bit-and r 0xFF) 16)
          (bit-or (bit-shift-left (bit-and g 0xFF) 8)
                 (bit-shift-left (bit-and b 0xFF) 0))))

(defn color-as-hex
  [rgb-color]
  (when rgb-color
    (let [[_ r g b] (re-find #"rgb\((\d+), (\d+), (\d+)\)" rgb-color)]
      (format "#%06X" (rgb-int-from-components (Integer. r) (Integer. g) (Integer. b))))))

(defn parse-font-size
  [font-size]
  (cond 
   (re-find #"(?i)(\d+)px$" font-size) (second (re-find #"(?i)(\d+)px$" font-size))
   (re-find #"^[a-zA-Z]$" font-size) font-size
   (re-find #"(?i)(.+)em$" font-size) (int (/ (Double. (second (re-find #"(?i)(.+)em$" font-size))) 
                                              (/ 1 16)))
   :else font-size))

;;Translators for each type of html tag
(defmulti translate (fn [node ancestry styles] (:tag node)))

(defmethod translate :body
  [node ancestry styles]
  (println "Translating body")
  (let [attrs (styles-for-node node ancestry styles)]
    [:mx:Application {:xmlns:mx "library://ns.adobe.com/flex/mx"
                      :xmlns:fx "http://ns.adobe.com/mxml/2009" 
                      :xmlns:s "library://ns.adobe.com/flex/spark" 
                      :backgroundColor (color-as-hex (:background-color attrs))
                      :width 1024
                      :height 768}
      (map #(translate % (cons node ancestry) styles) (children node))]))

(defmethod translate :article 
  [node ancestry styles]  
  (println "Translating article")
  (let [attrs (styles-for-node node ancestry styles)] 
    ;;if has child articles just draw a box, if 
    ;;leaf article draw a grid
    (if (seq (filter #(= (:tag %) :article) (children node)))
      (let [childs (children node)
            child-wo-header (filter #(not= (:tag %) :h2) childs)] ;;remove supeflous h2 tags present in nested arts
        [:mx:VBox {:backgroundColor (color-as-hex (:background-color attrs))}
         (map #(translate % (cons node ancestry) styles) child-wo-header)])
      (let [image (extract-image attrs)] 
        [:mx:VBox {:backgroundColor (color-as-hex (:background-color attrs))
                   :borderStyle "solid"
                   :width "100%"}
         (map #(translate % (cons node ancestry) styles) (children node))]))))

(defn translate-header-image
  "Creates an image cell to be used on an article header"
  [image align]
  [:mx:Image {:width 100
              :height 50
              :verticalAlign "middle"
              :source (format "@Embed(source='%s')" 
                              (:path image))}])

(defn translate-header-text
  "Creates a text cell to be used on an article header"
  [text attrs align]
  [:mx:Label {:width (if (= align :left) "850" "800")
              :text (inline-trim text)
              :fontSize (parse-font-size (:font-size attrs))
              :color (color-as-hex (:color attrs))
              :fontWeight "bold"
              :textAlign (or (:text-align attrs) (name align))}])

(defn translate-header
  "Creates a header with an optional image located on the right or
   on the left of the header. The text is aligned according to the 
   image position"
  [node attrs]
  (if-let [image (extract-image attrs)]
    (if (= :right (:position image))
      (seq [(translate-header-text (first (:content node)) attrs :left)
            (translate-header-image image :right)])
      (seq [(translate-header-image image :left)
            (translate-header-text (first (:content node)) attrs :right)]))
    (translate-header-text (first (:content node)) attrs :left)))

(defn build-single-row-header
  "Creates a header row using a single row-single header"
  [node ancestry styles]
  (let [attrs (styles-for-node node ancestry styles)]
    [:mx:HBox {:width "100%"
               :height "50"
               :verticalAlign "middle"
               :backgroundColor (color-as-hex (:background-color attrs))}
     [:mx:Label {:paddingLeft "5"}]
     (translate-header node attrs)]))

(defmethod translate :h2
  [node ancestry styles]
  (println "Translating h2 ")
  (build-single-row-header node ancestry styles))

(defmethod translate :h3
  [node ancestry styles]
  (println "Translating h3")
  (build-single-row-header node ancestry styles))

(defmethod translate :h4
  [node ancestry styles]
  (println "Translating h4")
  (map #(translate % (cons node ancestry) styles) (children node)))

(defmethod translate :table
  [node ancestry styles])

(defmethod translate :footer
  [node ancestry styles]
  (let [attrs (styles-for-node node ancestry styles)]
    (translate-header node attrs)))

(defmethod translate :hgroup
  [node ancestry styles]
  (println "Translating hgroup")
  (map #(translate % (cons node ancestry) styles) (children node)))

(defmethod translate :section
  [node ancestry styles]
  (println "Translating section")
  (map #(translate % (cons node ancestry) styles) (children node)))

(defmethod translate :span
  [node ancestry styles]
  (println "Translating span")
  (let [attrs (styles-for-node node ancestry styles)]
    [:s:span {:textDecoration (or (:text-decoration attrs) "none")
              :backgroundColor (color-as-hex (:background-color attrs))}
     (html/text node)]))

(defmethod translate :p
  [node ancestry styles]
  (println "Translating p")
  (let [attrs (styles-for-node node ancestry styles)
        childs (:content node)]
    [:mx:HBox {:width "100%" 
               :verticalAlign "middle" 
               :backgroundColor (color-as-hex (:background-color attrs))}
     [:mx:Label {:paddingLeft "5"}]
     [:s:RichEditableText {:editable "false"
                           :focusEnabled "false"
                           :width "850"
                           :fontSize (parse-font-size (:font-size attrs))
                           :fontWeight (:font-weight attrs)
                           :color (color-as-hex (:color attrs))}
      (map #(if (string? %)
              (inline-trim %)
              (translate % (cons node ancestry) styles)) childs)]]))

;;rotos:
;;108 -> color de fondo text => no parsea p:first-entry
;;120 -> image va abajo quedo a la izquierda
;;110 -> falta imagen, podria ser mas chico
;;119 -> falta raya abajo

(def ^:dynamic *compiler* "/Users/guilespi/Downloads/flex_sdk_4.6/bin/mxmlc")

(defn compile-source
  "From a proper mxml string create a filename.swf file in 
   the specified base-directory"
  [base-directory filename content]
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
        _ (println "Translating " filename)
        html-content (:html (second htmlfile))
        sheets (stylesheets html-content base-directory)
        styles (parse-stylesheets sheets)
        ;;assume only one body always present
        html-node (first (html/select html-content [:html]))
        body (first (html/select html-node [:body]))
        markup (translate body [html-node] styles)
        object (hiccup/html markup)
        base-name (clojure.string/replace filename #".html" "")]
    (compile-source base-directory base-name object)))


(defn build-directory
  "Convert a complete directory of html files to swf ones"
  [base-directory]
  (let [files (read-html-files base-directory)]
    (doall (map #(build-file % base-directory) files))))
