(ns html2swf.styles)

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

