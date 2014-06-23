(ns beyeran-rbm.rbm
  (:gen-class)
  (:use (incanter core stats io)))

(defn visible-state-to-hidden-probabilities 
  "Description "
  [rbm-w visible-state]
  (div 1 (plus 1 (exp (mmult (minus rbm-w)
                             (trans visible-state))))))

(defn hidden-state-to-visible-probabilities 
  "Description "
  [rbm-w hidden-state]
  (div 1 (plus 1 (exp (mmult (minus (trans rbm-w))
                             (trans hidden-state))))))

(defn sample-bernoulli-list 
  [probability-list]
  (matrix-map (fn [n] 
                (let [p (cond
                         (== n 1) 0.99999
                         (== n 0) 0.00001
                         :else n)]
                  (sample-binomial 1 :prob p)))
              probability-list))

(defn sample-bernoulli 
  "Takes a matrix MAT and returns a matrix sampled by the Bernoulli Distribution
   according to the probabilities in MAT"
  [mat]
  (matrix (sample-bernoulli-list mat)))

(defn configuration-goodness-gradient
  ""
  [visible-state hidden-state]
  (mmult hidden-state
         (div (trans visible-state)
              (second (dim visible-state)))))

(defn cd1
  ""
  [rbm-w visible-data]
  (let [visible-data-p (sample-bernoulli visible-data)
        h0             (sample-bernoulli (visible-state-to-hidden-probabilities rbm-w (trans visible-data-p)))
        vh0            (configuration-goodness-gradient visible-data-p h0)
        v1             (sample-bernoulli (hidden-state-to-visible-probabilities rbm-w (trans h0)))
        h1             (visible-state-to-hidden-probabilities rbm-w (trans v1))
        vh1            (configuration-goodness-gradient v1 h1)]
    (minus vh0 vh1)))


(defn rbm
  "Restricted Boltzmann Machine"
  [num-hidden training-data learning-rate n-iterations]
  (let [mini-batch-size 100
        momentum        0.9
        n               (second     (dim training-data))
        p               (first    (dim training-data))
        model           (mult
                         (minus 
                          (mult 2
                                (matrix (sample-uniform (* num-hidden
                                                           p))
                                        p))
                          1) 0.1)
        momentum-speed  (matrix 0 num-hidden p)]

    (loop [i 1]
      (when (< i n-iterations)
        (print "  -> Epoch: ")
        (print i)
        (print "\n")

        (def momentum-speed (plus (mult momentum momentum-speed) 
                                  (cd1 model training-data)))
        (def model (plus model
                         (mult momentum-speed learning-rate)))
        
        (recur (+ i 1))))    
    model))

(defn rbm-closure
  "A closure for the RBM. Text classification is a computational exhaustive task.
For better performance, the input is not read in a bulk, but as closure. 

NOTE: The parameter P can be interpreted as the vocabulary size.

EXAMPLE: (def rbm1 (rbm-closure 500 0.09 2000 2000))
         (rbm1 (matrix [0 0 0]) 2000)"
  [num-hidden learning-rate p iterations]
  (let [mini-batch-size 100
        momentum 0.9
        momentum-speed  (matrix 0 num-hidden p)]
    (fn [sample model]
      (def momentum-speed (plus (mult momentum momentum-speed) 
                                (cd1 model sample)))
      (plus model (mult momentum-speed learning-rate)))))
