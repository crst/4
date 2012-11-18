;; 78 "Reimplement Trampoline"
(fn [f x]
 (let [g (f x)]
   (loop [h g]
     (if (fn? h)
       (recur (h))
       h))))


;; 92 "Read Roman numberals"
(fn [rn]
 (let [v [[#"IV" "1"] [#"IX" "2"] [#"XL" "3"] [#"XC" "4"] [#"CD" "5"] [#"CM" "6"]]
       m {"1" 4 "2" 9 "3" 40 "4" 90 "5" 400 "6" 900 "I" 1 "V" 5 "X" 10 "L" 50 "C" 100 "D" 500 "M" 1000}]
  (apply + (map #(m (str %)) (reduce (fn [a [o n]] (clojure.string/replace a o n)) rn v)))))


;; 121 "Universal Computation Engine"
(fn [e]
  (let [op {'/ / '* * '+ + '- -}]
    (letfn [(f [expr b]
              (cond
               (list? expr) (apply (f (first expr) b) (map (fn [a] (f a b)) (rest expr)))
               (contains? op expr) (op expr)
               (contains? b expr) (b expr)
               :else expr))]
      (fn [b] (f e b)))))


;; 158 "Decurry"
(fn [f]
  (fn [x & rst]
    (loop [g (f x) args rst]
      (if (empty? args)
        g
        (recur (g (first args)) (rest args))))))


;; 164 "Language of a DFA"
(fn [dfa]
 (let [step (fn [[in st]] (map (fn [[c s]] [(str in c) s]) ((dfa :transitions) st)))
       run-dfa (fn f [states]
                 (let [new-states (mapcat step states)
                       acc-states (filter (fn [[i s]] (contains? (dfa :accepts) s)) new-states)
                       words (map first acc-states)]
                   (if (empty? states)
                     []
                     (lazy-seq (concat words (f new-states))))))]
   (run-dfa [["" (dfa :start)]])))
