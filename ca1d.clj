(def num-trial 32)
(def rule 90)
(def initial-state (flatten (list (repeat 32 0) 1 (repeat 32 0))))

(defn show-cells [cells]
  (println (apply str (map #(cond (= % 0) " "
                                  (= % 1) "*") cells))))

(defn neighbor [idx source]
  (if (= idx 0)
    (cons (last source) (first (split-at 2 source)))
    (first (split-at 3 (last (split-at (- idx 1) (cycle source)))))))

(defn neighbors [state]
  (map #(neighbor % state) (take (count state) (iterate inc 0))))

(defn pairs [seq]
  (if (> (count seq) 1)
    (cons (take 2 seq) (pairs (last (split-at 2 seq))))))

(defn pattern [n]
  (reduce + (map #(apply * %) (pairs (interleave [4 2 1] n)))))

(defn next-status [pat]
  (if (> (bit-and rule (bit-shift-left 1 pat)) 0) 1 0))

(defn tick [state]
  (map next-status (map pattern (neighbors state))))

(doseq [state (take num-trial (iterate tick initial-state))]
  (show-cells state))