(ns html2swf.translator
  (:require [html2swf.styles :refer [styles-for-node]]
            [net.cgrand.enlive-html :as html]
            [hiccup.core :as hiccup]
            [hiccup.util :as hutil])
  (:use [html2swf.utils]))

(def ^:dynamic *swf-width* 1024)
(def ^:dynamic *swf-height* 768)

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

(defn create-border
  [border]
  (when border
    (when-let [[_ size style color] (re-find #"([^ ]+) ([^ ]+) (.+)$" border)]
      {:size (parse-size size)
       :style style
       :color (color-as-hex color)})))

(defn parse-border
  [attrs]
  (cond 
   (:border attrs) (create-border (:border attrs))
   (:border-bottom attrs) (merge {:sides "bottom"} 
                                 (create-border (:border-bottom attrs)))
   (:border-top attrs) (merge {:sides "top"} 
                              (create-border (:border-top attrs)))))

(defn extract-image
  "Read other attributes of the image such as location (right bottom etc"
  [attrs]
  (when-let [background-image (or (:background-image attrs) (:background attrs))]
    ;;FIXME: this extraction is broken only works if images and CSS are in subdirectories
    ;;of the main path, probably to happen, but broken anyway
    (when-let [[_ image-path] (re-find #"url\(\.\./([^)]+)" background-image)]
      {:path image-path
       :position (if (and (:background-position attrs)
                          (re-find #"right" (:background-position attrs)))
                   :right
                   :left)})))

(defn translate-header-image
  "Creates an image cell to be used on an article header"
  [image align]
  (when image
    [:mx:Image {:width (or (:width image) (int (* *swf-width* 0.0976)))
                :height (or (:height image) (int (* *swf-height* 0.0651)))
                :verticalAlign "middle"
                :horizontalAlign (:align image)
                :source (format "@Embed(source='%s')" 
                                (:path image))}]))

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

(defn padded-image
  [image]
  (when image
    (let [img (translate-header-image image :any)
          pad [:mx:Label {:width (int (* *swf-width* 0.72))}]]
      [:mx:HBox
       (case (:position image)
         :left (seq [img pad])
         :right (seq [pad img]))])))

(defmethod translate :html
  [node ancestry styles]
  (println "Translating html")
  (let [attrs (styles-for-node node ancestry styles)
        props (:attrs node)
        ;;assume only one body always present
        body (first (html/select node [:body]))]
    [:mx:Application {:xmlns:mx "library://ns.adobe.com/flex/mx"
                      :xmlns:fx "http://ns.adobe.com/mxml/2009" 
                      :xmlns:s "library://ns.adobe.com/flex/spark" 
                      :backgroundColor (color-as-hex (:background-color attrs))
                      :layoutDirection (:dir props)
                      :direction (:dir props)
                      :width *swf-width*
                      :height *swf-height*}
     (translate-seq body (children body) ancestry styles)]))

(defmethod translate :body
  [node ancestry styles]
  (println "Translating body")
  (translate-seq node (children node) ancestry styles))

(defmethod translate :article 
  [node ancestry styles]  
  (println "Translating article")
  (let [attrs (styles-for-node node ancestry styles)
        nested-articles (seq (filter #(= (:tag %) :article) (children node)))
        border (parse-border attrs)
        image (extract-image attrs)] 
    [:s:BorderContainer {:borderStyle "solid"
                         :borderColor (:color border)
                         :borderVisible (if nested-articles "false" "true")
                         :borderWeight (:size border)
                         :width "100%"}
     [:mx:VBox {:backgroundColor (color-as-hex (:background-color attrs))
                :borderVisible "false"
                :width "100%"}
      (padded-image image)
      (translate-seq node (children node) ancestry styles)]]))

(defn translate-header-text
  "Creates a text cell to be used on an article header"
  [node ancestry styles]
  (let [attrs (styles-for-node node ancestry styles)
        text (html/text node)
        hide (= (:display attrs) "none")]
    (when (not hide)
      [:s:BorderContainer {:backgroundColor (color-as-hex (:background-color attrs))
                           :borderVisible "false"
                           :height (int (* *swf-height* 0.0651))
                           :width "100%"}
       [:mx:Label {:width (int (* *swf-width* 0.83))
                   :text text
                   :fontSize (parse-size (:font-size attrs))
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

(defmethod translate :img
  [node ancestry styles]
  (let [attrs (styles-for-node node ancestry styles)
        props (:attrs node)
        image {:path (:src props)
               :width (:width props)
               :height (:height props)
               :align (:text-align attrs)}]
    (translate-header-image image :any)))

(defn translate-text-seq
  [node ancestry styles]
  (let [attrs (styles-for-node node ancestry styles)] 
    (map-indexed (fn [idx child] 
                   (if (string? child)
                     [:mx:Label {:text (inline-trim (html/text node))
                                 :fontSize (parse-size (:font-size attrs))
                                 :textAlign (:text-algin attrs)}]
                     (translate child (cons {:index (inc idx)
                                             :node node} ancestry) styles))) (:content node))))

(defmethod translate :td
  [node ancestry styles]
  (let [attrs (styles-for-node node ancestry styles)]
    [:mx:GridItem {:backgroundColor (color-as-hex (:background-color attrs))
                   :color (color-as-hex (:color attrs))
                   :width "100%"}
     (translate-text-seq node ancestry styles)]))

(defmethod translate :th
  [node ancestry styles]
  (let [attrs (styles-for-node node ancestry styles)]
    [:mx:GridItem {:backgroundColor (color-as-hex (:background-color attrs))
                   :color (color-as-hex (:color attrs))}
     [:mx:Label {:text (inline-trim (html/text node))
                 :fontWeight "bold"
                 :fontSize (parse-size (:font-size attrs))}]]))

(defmethod translate :tr
  [node ancestry styles]
  [:mx:GridRow {:borderStyle "solid"
                :width "100%"}
   (translate-seq node (children node) ancestry styles)])

(defmethod translate :thead
  [node ancestry styles]
  (translate-seq node (children node) ancestry styles))

(defmethod translate :tbody
  [node ancestry styles]
  (translate-seq node (children node) ancestry styles))

(defmethod translate :table
  [node ancestry styles]
  [:mx:Grid {:horizontalGap "2" 
             :verticalGap "0" 
             :borderStyle "solid"
             :width "100%"}
   (translate-seq node (children node) ancestry styles)])

(defmethod translate :footer
  [node ancestry styles]
  (println "Translating footer")
  (translate-header-text node ancestry styles))

(defmethod translate :hgroup
  [node ancestry styles]
  (println "Translating hgroup")
  (let [attrs (styles-for-node node ancestry styles)
        image (extract-image attrs)]
    [:mx:VBox
     (padded-image image)
     (translate-seq node (children node) ancestry styles)]))

(defmethod translate :section
  [node ancestry styles]
  (println "Translating section")
  (let [attrs (styles-for-node node ancestry styles)
        border (parse-border attrs)]
    [:s:BorderContainer {:borderStyle "solid"
                         :borderColor (:color border)
                         :borderVisible (if border "true" "false")
                         :borderWeight (:size border)}
     [:mx:VBox {:backgroundColor (color-as-hex (:background-color attrs))
                :borderVisible "false"
                :width "100%"}
      (translate-seq node (children node) ancestry styles)]]))

(defmethod translate :li
  [node ancestry styles]
  (println "Translating li")
  [:s:li (html/text node)])

(defn translate-list
  [node ancestry styles list-type]
  (let [attrs (styles-for-node node ancestry styles)]
    [:s:HGroup {:width "100%" 
                :verticalAlign "middle"}
     [:s:RichEditableText {:editable "false"
                           :focusEnabled "false"
                           :width "100%"
                           :height "100%"
                           :backgroundColor (color-as-hex (:background-color attrs))
                           :fontSize (parse-size (:font-size attrs))
                           :fontWeight (:font-weight attrs)
                           :multiline "true"
                           :lineBreak "toFit"
                           :color (color-as-hex (:color attrs))}
      [:s:list {:listStyleType list-type}
       (translate-seq node (children node) ancestry styles)]]]))

(defmethod translate :ul
  [node ancestry styles]
  (println "Translating ul")
  (translate-list node ancestry styles nil))

(defmethod translate :ol
  [node ancestry styles]
  (println "Translating ol")
  (translate-list node ancestry styles "decimal"))

(defmethod translate :b
  [node ancestry styles]
  (println "Translating b")
  (let [attrs (styles-for-node node ancestry styles)]
    [:s:span {:fontWeight (or (:font-weight attrs) "bold")
              :textDecoration (or (:text-decoration attrs) "none")}
     (html/text node)]))

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
                           :width (int (* *swf-width* 0.83))
                           :fontSize (parse-size (:font-size attrs))
                           :fontWeight (:font-weight attrs)
                           :color (color-as-hex (:color attrs))}
      (translate-seq node childs ancestry styles)]]))

(defn translate-page 
  [html-content styles width height]
  (binding [*swf-width* width
            *swf-height* height] 
    (let [html-node (first (html/select html-content [:html]))
          attrs (:attrs html-node)
          markup (translate html-node [] styles)]
      (hiccup/html markup))))