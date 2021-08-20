(ns util.loader
  (:gen-class))
(require 'clojure.java.io)


(defn load [filename]
  (let [path (.getPath (clojure.java.io/resource filename))]
    (do
      (println "loading " path)
      (slurp path))))
