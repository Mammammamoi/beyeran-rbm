(ns beyeran-rbm.get-training
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:use (incanter core stats io)))

;;  (:require [clojure.java.io :as io])
;;  (:refer-clojure :exclude [* - + == /])
;;  (:use clojure.core.matrix)
;;  (:use clojure.core.matrix.operators))

;; note only 3000 words!

(defn read-file
  "Simply reads a text file returning each line as element of
a list. This function is the basis for the vocabulary reading:
each line is a word in the vocabulary"
  [path]
  (map clojure.string/trim
       (with-open [r (io/reader path)]
         (doall (line-seq r)))))

(defn read-as-map
  "Reads a text file and returns a map with the each line as
key and the line count as value. This represents the loaded
vocabulary list: Every line is a word (and now the key) and
its value represents the position in the vocabulary vector"
  [path]
  (apply hash-map
         (flatten
          (for [[k v] (map-indexed vector
                                   (read-file path))]
            [(keyword v) k]))))

(defn replace-item
  "Returns a list with the n-th item of l replaced by v."
  [l n v]
  (concat (take n l) (list v) (drop (inc n) l)))

(defn generate-vocab-vector
  "Takes a vocabulary map (obtained by READ-AS-MAP) and a string.
The string is then converted to a vocabulary vector."
  [vocab-map string]
  (let [;; clean and split string
        str (clojure.string/split 
             (clojure.string/trim string) #"\W+")
        ;; a vector containing only the positions in the vocab-map
        pos (filter #(not (nil? %))
                    (map (fn [n] (second (find vocab-map
                                               (keyword n))))
                         str))
        ;; a vector of zeros with the length of the vocab-map
        ;; (yes, it's pretty ugly)
        vec (apply vector (repeat (count vocab-map) 0))]
    ;; generating a vector with the lenght of the vocabulary.
    ;; Every element is zero, except the positions found in
    ;; POS
    (reduce (fn [v n] (assoc v n 1)) vec pos)))


(defn to-file
  "Writes the vocabulary to a file for later processing"
  [vocab-file input-file output-file]
  (println "  => reading input")
  (let [vocabulary (read-as-map vocab-file)
        lines (clojure.string/split (slurp input-file) #"\r\n")]
    (println "  => writing output")
    (spit output-file 
          (reduce (fn [a b] 
                    (str a (clojure.string/join "," (generate-vocab-vector vocabulary b)) "\r\n")) 
                  "" 
                  lines))
    :append true))
