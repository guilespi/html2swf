(ns html2swf.translator
  (:require [html2swf.styles :refer [styles-for-node]]
            [net.cgrand.enlive-html :as html]
            [hiccup.core :as hiccup])
  (:use [html2swf.utils]))


(defn parse-font-size
  [font-size]
  (cond 
   (re-find #"(?i)(\d+)px$" font-size) (second (re-find #"(?i)(\d+)px$" font-size))
   (re-find #"^[a-zA-Z]$" font-size) font-size
   (re-find #"(?i)(.+)em$" font-size) (int (/ (Double. (second (re-find #"(?i)(.+)em$" font-size))) 
                                              (/ 1 16)))
   :else font-size))

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

(defn children
  "Shortcut for using Enlive to get all elements beneath an element"
  [node]
  (filter map? (:content node)))

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

(defmethod translate :header
  [node ancestry styles]
  (println "Translating header")
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

(defn translate-page 
  [html-content styles]
  (let [html-node (first (html/select html-content [:html]))
        ;;assume only one body always present
        body (first (html/select html-node [:body]))
        markup (translate body [html-node] styles)]
    (hiccup/html markup)))