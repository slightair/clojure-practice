(def trials 32)
(def rule 90)
(def initial-state (concat (repeat 32 0) '(1) (repeat 32 0)))

(defn show-cells [cells]
  (println (apply str (map #(cond (= % 0) " "
                                  (= % 1) "*") cells))))

(defn neighbors [state]
  (concat
    (list (cons (last state) (first (split-at 2 state))))
    (partition 3 1 state)
    (list (concat (last (split-at (- (count state) 2) state)) (list (first state))))))

(defn pattern [n]
  (reduce + (map #(apply * %) (partition 2 (interleave [4 2 1] n)))))

(defn next-status [pat]
  (if (> (bit-and rule (bit-shift-left 1 pat)) 0) 1 0))

(defn tick [state]
  (map next-status (map pattern (neighbors state))))

(doseq [state (take trials (iterate tick initial-state))]
  (show-cells state))