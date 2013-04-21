(ns html2swf.main
  (:gen-class)
  (:require [html2swf.compiler :as compiler])
  (:use [clojure.tools.cli :only [cli]]))

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