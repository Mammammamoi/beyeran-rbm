(ns beyeran-rbm.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [beyeran-rbm.rbm :as rbm]
            [beyeran-rbm.get-training :as train])
  (:use (incanter core stats io)))


(defn make-training
  [vocab input-path]
  (matrix
   (map (fn [n] (train/generate-vocab-vector vocab (clojure.string/trim n)))
        (with-open [r (io/reader input-path)]
          (doall (line-seq r))))))

(defn train-by-file
  ""
  [vocabulary-file input-file num-hidden learning-rate iterations]
  (let [vocabulary (train/read-as-map vocabulary-file)
        p (count vocabulary)
        lines (clojure.string/split (slurp input-file) #"\r\n")
        model (mult (minus (mult 2 (matrix (sample-uniform (* num-hidden p))
                                           p)) 1) 0.1)
        rbm-c (rbm/rbm-closure num-hidden learning-rate p iterations)]
    (loop [i 1]
      (when (< i iterations)
        (print "  => epoch ")
        (print i)
        (print "\n")

        (reduce (fn [model n] (rbm-c (matrix (train/generate-vocab-vector vocabulary n)) model)) 
                model lines)
        (recur (+ i 1))))
    model))

;; e.g: (def weights (train-by-file ....))
