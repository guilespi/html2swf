(ns html2swf.main
  (:gen-class)
  (:require [html2swf.compiler :as compiler])
  (:use [clojure.tools.cli :only [cli]]))

;;rotos:
;;120 -> image va abajo quedo a la izquierda
;;110, 4 -> falta imagen, podria ser mas chico
;;119 -> falta raya abajo
;;115 -> imagen va arriba
;;114 y 116 no parsea la imagen del gradiente loco
;;103 la imagen quedo en cualquier lado
;;102 -> faltan las imagenes del cabezal de mail
;;3 -> falta la rayita abajo
;;112 -> imagen un poco corrida 
;;101 -> falta la imagen sobre la derecha del satelite que antes estaba

(defn -main
  "Application entry point, should receive the directory to compile"
  [& args]
  (let [[options args banner] (cli args
                                   ["-d" "--directory" "Directory where files to be built are located" :default "."]
                                   ["-w" "--width" "Width of the swf to be compiled" :default 1024]
                                   ["-h" "--height" "Height of the swf to be compiled" :default 768]
                                   ["-f" "--file" "File to compile if you want only one"])]
    (if (:file options)
      (if (compiler/build-file (:file options) (Integer. (:width options)) (Integer. (:height options)))
        "Success"
        "Failed")
      (let [result (compiler/build-directory (:directory options) 
                                             (Integer. (:width options)) 
                                             (Integer. (:height options)))]
        (println (format "Compiled %s files %s success %s failed"
                         (count result)
                         (count (filter true? result))
                         (count (filter false? result))))))))