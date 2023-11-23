(ns mmp.task2.task-2-2)

(defn trapezia-area [f t_1 t_2]
  (* (- t_1 t_2) (/ (+ (f t_1) (f t_2)) 2)))

(defn partial-sum-seq [f step]
  (reductions + 0 (map #(trapezia-area f (+ step %) %) (iterate #(+ step %) 0))))

(defn calc-integral-seq
  [f step]
  (let [integrate-seq (partial-sum-seq f step)]
    (fn [x]
      (nth integrate-seq (quot x step)))))

(defn -main
  []
  (println "Sequenced")
  (let [integrate-seq (calc-integral-seq #(do (Thread/sleep 1) (* 4 %)) 1)]
    (time (integrate-seq 500))
    (time (integrate-seq 500.1))
    (time (integrate-seq 500))
    (time (integrate-seq 500)))
  )

