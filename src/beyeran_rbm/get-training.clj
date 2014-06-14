(ns beyeran-rbm.get-training
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:use (incanter core stats io)))

(defn read-file
  [path]
  (with-open [r (io/reader path)]
    (doall (line-seq r))))

;; note only 3000 words!
