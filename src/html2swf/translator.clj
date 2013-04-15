(ns html2swf.translator
  (:require [html2swf.styles :refer [styles-for-node]]
            [net.cgrand.enlive-html :as html]
            [hiccup.core :as hiccup]
            [hiccup.util :as hutil])
  (:use [html2swf.utils]))

(defn inline-trim
  [^String string]
  (-> (clojure.string/replace string #"\s\s+" " ")
      (hutil/escape-html)))

(defn parse-size
  [size]
  (when size
    (cond 
     (re-find #"(?i)(\d+)px$" size) (second (re-find #"(?i)(\d+)px$" size))
     (re-find #"^[a-zA-Z]$" size) size
     (re-find #"(?i)(.+)em$" size) (int (/ (Double. (second (re-find #"(?i)(.+)em$" size))) 
                                           (/ 1 16)))
     :else size)))

(defn parse-border
  [border]
  (when border
    (when-let [[_ size style color] (re-find #"([^ ]+) ([^ ]+) (.+)$" border)]
      {:size (parse-size size)
       :style style
       :color (color-as-hex color)})))

(defn extract-image
  "Read other attributes of the image such as location (right bottom etc"
  [attrs]
  (when-let [background-image (:background-image attrs)]
    ;;FIXME: this extraction is broken only works if images and CSS are in subdirectories
    ;;of the main path, probably to happen, but broken anyway
    (when-let [[_ image-path] (re-find #"url\(\.\./([^)]+)" background-image)]
      {:path image-path
       :position (if (and (:background-position attrs)
                          (re-find #"left" (:background-position attrs)))
                   :left
                   :right)})))

(defn children
  "Shortcut for using Enlive to get all elements beneath an element"
  [node]
  (filter map? (:content node)))

;;Translators for each type of html tag
(defmulti translate (fn [node ancestry styles] (:tag node)))

(defn translate-seq
  [parent childs ancestry styles]
  (map-indexed (fn [idx child]
                 (if (string? child)
                   (inline-trim child)
                   (translate child (cons {:index (inc idx)
                                           :node parent} ancestry) styles))) 
               childs))

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
     (translate-seq node (children node) ancestry styles)]))

(defmethod translate :article 
  [node ancestry styles]  
  (println "Translating article")
  (let [attrs (styles-for-node node ancestry styles)
        nested-articles (seq (filter #(= (:tag %) :article) (children node)))
        border (parse-border (:border attrs))] 
      (let [image (extract-image attrs)]
        [:s:BorderContainer {:borderStyle "solid"
                             :borderColor (:color border)
                             :borderVisible (if nested-articles "false" "true")
                             :borderWeight (:size border)}
         [:mx:VBox {:backgroundColor (color-as-hex (:background-color attrs))
                    :borderVisible "false"
                    :width "100%"}
          (translate-seq node (children node) ancestry styles)]])))

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
  [node ancestry styles]
  (let [attrs (styles-for-node node ancestry styles)
        text (html/text node)
        hide (= (:display attrs) "none")]
    (when (not hide)
      [:s:BorderContainer {:backgroundColor (color-as-hex (:background-color attrs))
                           :borderVisible "false"
                           :height 50}
       [:mx:Label {:width "850"
                   :text text
                   :fontSize (parse-size(:font-size attrs))
                   :color (color-as-hex (:color attrs))
                   :fontWeight "bold"
                   :textAlign (:text-align attrs)}]])))

(defn translate-header
  "Creates a header with an optional image located on the right or
   on the left of the header. The text is aligned according to the 
   image position"
  [node attrs ancestry styles]
  (if-let [image (extract-image attrs)]
    (if (= :right (:position image))
      (seq [(translate-seq node (children node) ancestry styles)
            (translate-header-image image :right)])
      (seq [(translate-header-image image :left)
            (translate-seq node (children node) ancestry styles)]))
    (translate-seq node (children node) ancestry styles)))

(defn build-single-row-header
  "Creates a header row using a single row-single header"
  [node ancestry styles]
  (let [attrs (styles-for-node node ancestry styles)]
    [:mx:HBox {:width "100%"
               :verticalAlign "middle"
               :backgroundColor (color-as-hex (:background-color attrs))}
     [:mx:Label {:paddingLeft "5"}]
     (translate-header node attrs ancestry styles)]))

(defmethod translate :h2
  [node ancestry styles]
  (println "Translating h2")
  (translate-header-text node ancestry styles))

(defmethod translate :h3
  [node ancestry styles]
  (println "Translating h3")
  (translate-header-text node ancestry styles))

(defmethod translate :h4
  [node ancestry styles]
  (println "Translating h4")
  (translate-header-text node ancestry styles))

(defmethod translate :header
  [node ancestry styles]
  (println "Translating header")
  (build-single-row-header node ancestry styles))

(defmethod translate :colgroup
  [node ancestry styles])

(defmethod translate :td
  [node ancestry styles]
  [:mx:GridItem
   [:mx:Label {:text (inline-trim (html/text node))}]])

(defmethod translate :th
  [node ancestry styles]
  [:mx:GridItem
   [:mx:Label {:text (inline-trim (html/text node))
               :fontWeight "bold"}]])

(defmethod translate :tr
  [node ancestry styles]
  [:mx:GridRow
   (translate-seq node (children node) ancestry styles)])

(defmethod translate :tbody
  [node ancestry styles]
  [:mx:Grid
   (translate-seq node (children node) ancestry styles)])

(defmethod translate :table
  [node ancestry styles]
  [:mx:VBox
   (translate-seq node (children node) ancestry styles)])

(defmethod translate :footer
  [node ancestry styles]
  (println "Translating footer")
  (translate-header-text node ancestry styles))

(defmethod translate :hgroup
  [node ancestry styles]
  (println "Translating hgroup")
  [:mx:VBox
   (translate-seq node (children node) ancestry styles)])

(defmethod translate :section
  [node ancestry styles]
  (println "Translating section")
  (translate-seq node (children node) ancestry styles))

(defmethod translate :li
  [node ancestry styles]
  (println "Translating li")
  [:s:li (html/text node)])

(defmethod translate :ul
  [node ancestry styles]
  (println "Translating ul")
  (let [attrs (styles-for-node node ancestry styles)]
    [:mx:HBox {:width "100%" 
               :verticalAlign "middle" 
               :backgroundColor (color-as-hex (:background-color attrs))}
     [:s:RichEditableText {:editable "false"
                           :focusEnabled "false"
                           :width "850"
                           :fontSize (parse-size (:font-size attrs))
                           :fontWeight (:font-weight attrs)
                           :color (color-as-hex (:color attrs))}
      [:s:list 
       (translate-seq node (children node) ancestry styles)]]]))

(defmethod translate :b
  [node ancestry styles]
  (println "Translating b")
  [:s:span {:fontWeight "bold"}
   (html/text node)])

(defmethod translate :span
  [node ancestry styles]
  (println "Translating span")
  (let [attrs (styles-for-node node ancestry styles)]
    [:s:span {:textDecoration (or (:text-decoration attrs) "none")
              :backgroundColor (color-as-hex (:background-color attrs))}
     (html/text node)]))

(defmethod translate :p
  [node ancestry styles]
  (println "Translating p with index" (:index (first ancestry)))
  (let [attrs (styles-for-node node ancestry styles)
        childs (:content node)]
    [:mx:HBox {:width "100%" 
               :verticalAlign "middle" 
               :backgroundColor (color-as-hex (:background-color attrs))}
     [:mx:Label {:paddingLeft "5"}]
     [:s:RichEditableText {:editable "false"
                           :focusEnabled "false"
                           :width "850"
                           :fontSize (parse-size (:font-size attrs))
                           :fontWeight (:font-weight attrs)
                           :color (color-as-hex (:color attrs))}
      (translate-seq node childs ancestry styles)]]))

(defn translate-page 
  [html-content styles]
  (let [html-node (first (html/select html-content [:html]))
        ;;assume only one body always present
        body (first (html/select html-node [:body]))
        markup (translate body [{:index 1 :node html-node}] styles)]
    (hiccup/html markup)))