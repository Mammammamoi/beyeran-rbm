(ns beyeran-rbm.core
  (:gen-class)
  (:use (incanter core stats io)))


(defn visible-state-to-hidden-probabilities 
  "Description "
  [rbm-w visible-state]
  (div 1 (plus 1 (exp (mult (minus rbm-w)
                            visible-state)))))

(defn hidden-state-to-visible-probabilities 
  "Description "
  [rbm-w hidden-state]
  (div 1 (plus 1 (exp (mult (minus (trans rbm-w))
                            hidden-state)))))

(defn sample-bernoulli-list 
  [probability-list]
  (matrix-map (fn [n] (sample-binomial 1 :prob n)) probability-list))

(defn sample-bernoulli 
  "Takes a matrix MAT and returns a matrix sampled by the Bernoulli Distribution
   according to the probabilities in MAT"
  [mat]
  (let [dims (dim mat)]
    (matrix (sample-bernoulli-list mat))))

(defn configuration-goodness-gradient
  ""
  [visible-state hidden-state]
  (mult hidden-state
        (div (trans visible-state)
             (second (dim visible-state)))))

(defn cd1
  ""
  [rbm-w visible-data]
  (let [visible-data-p (sample-bernoulli visible-data)
        h0             (sample-bernoulli (visible-state-to-hidden-probabilities rbm-w visible-data-p))
        vh0            (configuration-goodness-gradient visible-data-p h0)
        v1             (sample-bernoulli (hidden-state-to-visible-probabilities rbm-w h0))
        h1             (visible-state-to-hidden-probabilities rbm-w v1)
        vh1            (configuration-goodness-gradient v1 h1)]
    (minus vh0 vh1)))

(defn rbm
  ""
  [num-hidden training-data learning-rate n-iterations]
  (let [mini-batch-size 100
        momentum        0.9
        n               (first   (dim training-data))
        p               (second  (dim training-data))
        model           (mult
                         (minus 
                          (mult 2
                                (matrix (sample-uniform (* num-hidden
                                                           p))))
                          1) 0.1)
        momentum-speed  (matrix 0 num-hidden p)]
    (for [i (range n-iterations)]
      (fn []
        (def momentum-speed (plus (mult momentum momentum-speed) 
                                  (cd1 model training-data)))
        (def model (plus model
                         (mult momentum-speed learning-rate)))
        (inc i)))
    (matrix model p)))

;; (defn -main
;;   "I don't do a whole lot ... yet."
;;   [& args]
;;   (println "Hello, World!"))
