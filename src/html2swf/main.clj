(ns html2swf.main
  (:gen-class)
  (:require [html2swf.compiler :as compiler])
  (:use [clojure.tools.cli :only [cli]]))

;;rotos:
;;108 -> color de fondo text => no parsea p:first-entry
;;120 -> image va abajo quedo a la izquierda
;;110 -> falta imagen, podria ser mas chico
;;119 -> falta raya abajo

(defn -main
  "Application entry point, should receive the directory to compile"
  [& args]
  (let [[options args banner] (cli args
                     ["-d" "--directory" "Directory where files to be built are located" :default "."])]
    (compiler/build-directory (:directory options))))