(ns html2swf.translator
  (:require [html2swf.styles :refer [styles-for-node]]
            [net.cgrand.enlive-html :as html]
            [hiccup.core :as hiccup]
            [hiccup.util :as hutil]
            [clojure.java.io :as io])
  (:use [html2swf.utils]))

(def ^:dynamic *swf-width* 1024)
(def ^:dynamic *swf-height* 768)
(def ^:dynamic *base-directory* nil)
(def ^:dynamic *inline-block* false)

(defn inline-trim
  [^String string]
  (let [trimmed (clojure.string/replace string #"\s\s+" " ")]
    (if *inline-block*
      (hutil/escape-html trimmed)
      trimmed)))

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
   (:border-bottom attrs) (merge {:sides :bottom} 
                                 (create-border (:border-bottom attrs)))
   (:border-top attrs) (merge {:sides :top} 
                              (create-border (:border-top attrs)))))

(defn parse-font-family
  [font-family]
  (clojure.string/replace font-family #"\"" ""))

(defn extract-image
  "Read other attributes of the image such as location (right bottom etc"
  [attrs]
  (when-let [background-image (or (:background-image attrs) (:background attrs))]
    ;;css urls come fixed from the css parser to absolute paths
    (when-let [[_ image-path] (re-find #"url\(([^)]+)\)" background-image)]
      {:path image-path
       :position (if (and (:background-position attrs)
                          (re-find #"right" (:background-position attrs)))
                   :right
                   :left)})))

(defn normalize-image
  [image]
  (let [original-file (io/file (:path image))
        svg-content (slurp original-file)
        normalized (-> svg-content
                       ;;remove viewboxes starting at 0,0
                       (clojure.string/replace #"viewBox=\"0\s0[^\"]+\"" "")
                       ;;fix invalid paths ending in kz
                       (clojure.string/replace #"(?i)(path d=\"[^k]+)kz" #(str (get %1 1) "Z")))
        [_ relative-path separator filename] (re-find #"(.+)([/\\])([^/\\]+.svg)$" (:path image))
        tmp-dir (System/getProperty "java.io.tmpdir")
        normalized-path (str tmp-dir filename)]
    (try
      (spit normalized-path normalized)
      (assoc image :path normalized-path)
      (catch Exception e
        (println "Exception normalizing image" (:path image))
        image))))

(defn translate-inline-image
  [image align]
  (let [align (or (:align image) align)]
    (when (and image (.exists (io/file (:path image))))
      (let [image (normalize-image image)]
        [:s:img {:percentWidth (when (not (:width image)) "20")
                 :width (:width image)
                 :height (or (:height image) (int (* *swf-height* 0.0651)))
                 :verticalAlign "middle"
                 :float (when (contains? #{"left" "right"} align) align)
                 :source (format "@Embed(source='%s')" 
                                 (:path image))}]))))

(defn translate-block-image
  "Creates an image cell to be used on an article header"
  [image align]
  (when (and image (.exists (io/file (:path image))))
    (let [image (normalize-image image)]
      [:mx:Image {:percentWidth (when (not (:width image)) "20")
                  :width (:width image)
                  :height (or (:height image) (int (* *swf-height* 0.0651)))
                  :verticalAlign "middle"
                  :horizontalAlign (or (:align image) align)
                  :source (format "@Embed(source='%s')" 
                                  (clojure.string/replace (:path image)
                                                          #"\\"
                                                          "\\\\\\\\"))}])))

(defn translate-image 
  [image align]
  (if *inline-block*
    (translate-inline-image image align)
    (translate-block-image image align)))

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
                   (let [text (inline-trim child)]
                     (if (= idx 0) 
                       (clojure.string/triml text)
                       text))
                   (translate child (cons {:index (inc idx)
                                           :node parent} ancestry) styles))) 
               childs))

(defn padded-image
  [image]
  (when image
    (let [img (translate-block-image image (:position image))
          pad [:mx:Label {:percentWidth "80"}]]
      [:mx:HBox {:percentWidth "100"}
       (case (:position image)
         :left (seq [img pad])
         :right (seq [pad img]))])))

(defn single-border-line
  [border side]
  (when (and (= side (:sides border))
             (:size border)
             (> (Integer. (:size border)) 0))    
    [:s:HGroup {:percentWidth "100" :height (:size border)}
     [:mx:Label {:percentWidth "3"}]
     [:s:SkinnableContainer {:backgroundColor (or (:color border) "black")
                             :percentWidth "94"
                             :height (:size border)}]]))

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
                      :height "100%"
                      :width "100%"
                      :minWidth *swf-width*
                      :minHeight *swf-height*}
     (translate-seq body (children body) [{:node node :index 1}] styles)]))

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
                         :percentWidth "100"}
     [:mx:VBox {:backgroundColor (color-as-hex (:background-color attrs))
                :borderVisible "false"
                :percentWidth "100"}
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
                           :percentWidth "100"}
       [:mx:Label {:percentWidth "100"
                   :text text
                   :fontFamily (parse-font-family (:font-family attrs))
                   :fontSize (parse-size (:font-size attrs))
                   :color (color-as-hex (:color attrs))
                   :fontWeight (or (:font-weight attrs) "bold")
                   :textAlign (:text-align attrs)}]])))

(defn translate-header
  "Creates a header with an optional image located on the right or
   on the left of the header. The text is aligned according to the 
   image position"
  [node attrs ancestry styles]
  (if-let [image (extract-image attrs)]
    (if (= :right (:position image))
      (seq [(translate-seq node (children node) ancestry styles)
            (translate-block-image image :right)])
      (seq [(translate-block-image image :left)
            (translate-seq node (children node) ancestry styles)]))
    (translate-seq node (children node) ancestry styles)))

(defn build-single-row-header
  "Creates a header row using a single row-single header"
  [node ancestry styles]
  (let [attrs (styles-for-node node ancestry styles)
        border (parse-border attrs)]
    (seq [[:mx:HBox {:percentWidth "100"
                     :verticalAlign "middle"
                     :backgroundColor (color-as-hex (:background-color attrs))}
           [:mx:Label {:percentWidth "3"}]
           (translate-header node attrs ancestry styles)]
          (single-border-line border :bottom)])))

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
        image {:path (str *base-directory* (:src props))
               :width (:width props)
               :height (:height props)
               :align (:text-align attrs)}]
    (translate-image image nil)))

(defn translate-text-seq
  [node ancestry styles]
  (let [attrs (styles-for-node node ancestry styles)] 
    (map-indexed (fn [idx child] 
                   (if (string? child)
                     (if *inline-block*
                       child
                       [:mx:Label {:text (clojure.string/trim child)
                                   :fontSize (parse-size (:font-size attrs))
                                   :fontFamily (parse-font-family (:font-family attrs))
                                   :textAlign (:text-align attrs)}])
                     (translate child (cons {:index (inc idx)
                                             :node node} ancestry) styles))) (:content node))))

(defmethod translate :td
  [node ancestry styles]
  (let [attrs (styles-for-node node ancestry styles)
        border (parse-border attrs)]
    [:mx:GridItem {:backgroundColor (color-as-hex (:background-color attrs))
                   :color (color-as-hex (:color attrs))
                   :borderStyle (when border "solid")
                   :percentWidth "100"
                   :horizontalAlign (:text-align attrs)}
     (translate-text-seq node ancestry styles)]))

(defmethod translate :th
  [node ancestry styles]
  (let [attrs (styles-for-node node ancestry styles)
        border (parse-border attrs)]
    [:mx:GridItem {:backgroundColor (color-as-hex (:background-color attrs))
                   :color (color-as-hex (:color attrs))
                   :borderStyle (when border "solid")
                   :horizontalAlign (:text-align attrs)}
     [:mx:Label {:text (inline-trim (html/text node))
                 :fontWeight "bold"
                 :fontFamily (parse-font-family (:font-family attrs))
                 :fontSize (parse-size (:font-size attrs))}]]))

(defmethod translate :tr
  [node ancestry styles]
  (let [attrs (styles-for-node node ancestry styles)
        border (parse-border attrs)]
    [:mx:GridRow {:percentWidth "100"
                  :borderStyle (when border "solid")}
     (translate-seq node (children node) ancestry styles)]))

(defmethod translate :thead
  [node ancestry styles]
  (translate-seq node (children node) ancestry styles))

(defmethod translate :tbody
  [node ancestry styles]
  (translate-seq node (children node) ancestry styles))

(defmethod translate :table
  [node ancestry styles]
  (let [attrs (styles-for-node node ancestry styles)
        border (parse-border attrs)]
    [:mx:Grid {:horizontalGap "0" 
               :verticalGap "0" 
               :borderStyle (when border "solid")
               :borderColor (:color border)
               :percentWidth "100"}
     (translate-seq node (children node) ancestry styles)]))


(defmethod translate :dl
  [node ancestry styles]
  (println "Translating dl")
  [:mx:VBox {:percentWidth "94"}
   (loop [definitions (children node) entries []]
     (if (seq definitions)
       (let [dt (first definitions)
             dt-attrs (styles-for-node dt (conj ancestry node) styles)
             dd (second definitions)
             dd-attrs (styles-for-node dd (conj ancestry node) styles)
             row [:mx:HBox 
                  [:mx:Label {:text (inline-trim (html/text dt))
                              :fontWeight (:font-weight dt-attrs)
                              :fontFamily (parse-font-family (:font-family dt-attrs))
                              :fontSize (parse-size (:font-size dt-attrs))}]
                  [:mx:Label {:text (inline-trim (html/text dd))
                              :fontWeight (:font-weight dd-attrs)
                              :fontFamily (parse-font-family (:font-family dd-attrs))
                              :fontSize (parse-size (:font-size dd-attrs))}]]]
         (recur (drop 2 definitions) (conj entries row)))
       (seq entries)))])

(defmethod translate :footer
  [node ancestry styles]
  (println "Translating footer")
  (translate-header-text node ancestry styles))

(defmethod translate :div
  [node ancestry styles]
  (println "Translating div")
  (let [attrs (styles-for-node node ancestry styles)
        image (extract-image attrs)]
    [:mx:HBox {:percentWidth "94"} ;;needs to consider margin
     (translate-seq node (children node) ancestry styles)]))

(defmethod translate :hgroup
  [node ancestry styles]
  (println "Translating hgroup")
  (let [attrs (styles-for-node node ancestry styles)
        image (extract-image attrs)]
    [:mx:VBox {:percentWidth "94"} ;;needs to consider margin
     (padded-image image)
     (translate-seq node (children node) ancestry styles)]))

(defmethod translate :section
  [node ancestry styles]
  (println "Translating section")
  (let [attrs (styles-for-node node ancestry styles)
        border (parse-border attrs)]
    [:s:BorderContainer {:borderStyle "solid"
                         :borderColor (:color border)
                         :borderVisible (if (and border (not (:sides border))) "true" "false")
                         :borderWeight (:size border)
                         :percentWidth "100"}
     [:mx:VBox {:backgroundColor (color-as-hex (:background-color attrs))
                :borderVisible "false"
                :percentWidth "100"}
      (seq [(translate-seq node (children node) ancestry styles)
            (single-border-line border :bottom)])]]))

(defmethod translate :li
  [node ancestry styles]
  (println "Translating li")
  (let [attrs (styles-for-node node ancestry styles)]
    (binding [*inline-block* true]
      [:s:li 
       [:s:p {:textAlign (:text-align attrs)}
        (doall ;;never lazy evaluate outside bindings
         (translate-text-seq node ancestry styles))]])))

(defn translate-list
  [node ancestry styles list-type]
  (let [attrs (styles-for-node node ancestry styles)]
    [:s:HGroup {:width "100%" 
                :verticalAlign "middle"}
     [:s:RichEditableText {:editable "false"
                           :focusEnabled "false"
                           :percentWidth "94"
                           :paddingLeft "20"
                           :backgroundColor (color-as-hex (:background-color attrs))
                           :fontSize (parse-size (:font-size attrs))
                           :fontFamily (parse-font-family (:font-family attrs))
                           :fontWeight (:font-weight attrs)
                           :multiline "true"
                           :lineBreak "toFit"
                           :color (color-as-hex (:color attrs))}
      [:s:list {:listStyleType list-type :listStylePosition "outside"}
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
    (if *inline-block*
      [:s:span {:fontWeight (or (:font-weight attrs) "bold")
                :fontFamily (parse-font-family (:font-family attrs))
                :textDecoration (or (:text-decoration attrs) "none")}
       (html/text node)]
      [:mx:Label {:fontWeight (or (:font-weight attrs) "bold")
                  :text (html/text node)
                  :fontSize (parse-size (:font-size attrs))
                  :fontFamily (parse-font-family (:font-family attrs))}])))

(defmethod translate :span
  [node ancestry styles]
  (println "Translating span")
  (let [attrs (styles-for-node node ancestry styles)]
    (if *inline-block*
      [:s:span {:textDecoration (or (:text-decoration attrs) "none")
                :fontFamily (parse-font-family (:font-family attrs))
                :backgroundColor (color-as-hex (:background-color attrs))}
       (html/text node)]
      (translate-header-text node ancestry styles))))

(defmethod translate :p
  [node ancestry styles]
  (println "Translating p")
  (let [attrs (styles-for-node node ancestry styles)
        childs (:content node)]
    [:s:HGroup {:percentWidth "100" 
                :verticalAlign "middle"}
     [:mx:Label {:percentWidth "3"}]
     [:s:RichEditableText {:editable "false"
                           :focusEnabled "false"
                           :percentWidth "94"
                           :paddingTop "2"
                           :fontFamily (parse-font-family (:font-family attrs))
                           :fontSize (parse-size (:font-size attrs))
                           :fontWeight (:font-weight attrs)
                           :textAlign (:text-align attrs)
                           :color (color-as-hex (:color attrs))
                           :backgroundColor (color-as-hex (:background-color attrs))}
      (binding [*inline-block* true] 
        (doall (translate-seq node childs ancestry styles)))]]))

(defn translate-page 
  [html-content styles width height base-directory]
  (binding [*swf-width* width
            *swf-height* height
            *base-directory* base-directory] 
    (let [html-node (first (html/select html-content [:html]))
          attrs (:attrs html-node)
          markup (translate html-node [] styles)]
      (hiccup/html markup))))