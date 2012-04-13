(doseq [x (range 1 100)]
  (println
    (cond (= (rem x 15) 0) "FizzBuzz"
          (= (rem x 3) 0) "Fizz"
          (= (rem x 5) 0) "Buzz"
          :else x)))