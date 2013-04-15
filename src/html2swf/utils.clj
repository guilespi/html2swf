(ns html2swf.utils)

(defn debomify
  "Remove the BOM UTF-8 character from the beggining of a string"
  [^String line]
  (let [bom "\uFEFF"]
    (if (.startsWith line bom)
      (.substring line 1)
      line)))

(defn dos2unix
  "Remove return feed character from a string"
  [^String content]
  (clojure.string/replace content #"\r" ""))

(defn rgb-int-from-components
  "Convert a vector of the 3 rgb integer values into a color given in
  numeric rgb format"
  [r g b]
  (bit-or (bit-shift-left (bit-and r 0xFF) 16)
          (bit-or (bit-shift-left (bit-and g 0xFF) 8)
                 (bit-shift-left (bit-and b 0xFF) 0))))

(defn color-as-hex
  "From a rgb-color string of the form rgb(x, y, z) create a color
   hex string of the form #FFC600"
  [rgb-color]
  (when rgb-color
    (let [[_ r g b] (re-find #"rgb\((\d+), (\d+), (\d+)\)" rgb-color)]
      (format "#%06X" (rgb-int-from-components (Integer. r) (Integer. g) (Integer. b))))))
