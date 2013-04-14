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

(defmulti partial-condition-match? (fn [condition node ancestry]
                                     (cond 
                                      (re-find #"[^:]:(.+)$" condition) ::special-function
                                      (re-find #"^#.+" condition) ::selector-id
                                      (re-find #"^[-_a-zA-Z0-9]+\.[-_a-zA-Z0-9]+$" condition) ::selector-classed-tag
                                      (re-find #"^\.[-_a-zA-Z0-9]+$" condition) ::selector-class
                                      (re-find #"^[-_a-zA-Z0-9]+$" condition) ::selector-tag)))

(defn selector-function-match?
  [f n parents]
  true)

;;this are the selector:nth-of-type kind of selectors
;;first need to match first side traditionally then check if the function is true
(defmethod partial-condition-match? ::special-function
  [condition node ancestry]
  (when-let [[_ selector function] (re-find #"([^:]):(.+)$" condition)]
    (when (partial-condition-match? selector node)
      (selector-function-match? function node ancestry))))

(defmethod partial-condition-match? ::selector-id
  [condition node ancestry]
  (let [id (:id (:attrs node))]
    (= condition (str "#" id))))

(defmethod partial-condition-match? ::selector-class
  [condition node ancestry]
  (when-let [[_ class] (re-find #"^\.([-_a-zA-Z0-9]+)$" condition)] 
    (has-class? node class)))

(defmethod partial-condition-match? ::selector-tag
  [condition node ancestry]
  (= condition (name (:tag node))))

(defmethod partial-condition-match? ::selector-classed-tag
  [condition node ancestry]
  (when-let [[_ tag class] (re-find #"^([-_a-zA-Z0-9]+)\.([-_a-zA-Z0-9]+)$" condition)] 
    (and (= tag (name (:tag node)))
         (has-class? node class))))

(defmethod partial-condition-match? :default
  [condition node ancestry]
  (println (format "Unable to identify css selector '%s'" condition)))

(defn selector-match?
  "If the last element of the selector matches the specified node, returns the
   trimmed selector and ancestry (for the case of the '>' relationship).
   That is the selector [body #id1 ul] matches for the node :ul"
  [node selector ancestry]
  (cond
   (nil? node) false
   (nil? selector) true
   (not (seq selector)) true
   :else  (when (partial-condition-match? (last selector) node ancestry)
            ;;if theres a strict relationship, validate next parent also matches
            ;;and trim the selector and the parent
            (if (= ">" (last (butlast selector)))
              (when (partial-condition-match? (nth (reverse selector) 2 "") 
                                              (first ancestry)
                                              (rest ancestry))
                {:a (rest ancestry)
                 :s (drop-last 3 selector)})
              {:a ancestry
               :s (drop-last 1 selector)}))))

(defn trim-ancestry
  ;;drop parents until found one matching the next selector
  ;;eg. "body div ul.menu" may have bogus nodes in the hierarchy
  ;;drop-while is not used since access to the rest of the list is needed
  ;;in each iteration
  [selector nodes]
  (loop [ancestry nodes]
    (when (seq ancestry)
      (if (selector-match? (first ancestry) selector (rest ancestry))
        ancestry
        (recur (rest ancestry))))))

(defn selector-applies?
  "Returns true if the selector applies for the current node"
  [node ancestry selector]
  (loop [n node parents ancestry s selector]
    ;;current node matches for last selector tag
    (when-let [{new-selector :s new-parents :a} (selector-match? n s parents)]
      ;;if no more selectors found, this is a WIN
      (if (seq new-selector)
        (let [ancestry (trim-ancestry new-selector new-parents)] 
          (recur (first ancestry) (rest ancestry) new-selector))
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

