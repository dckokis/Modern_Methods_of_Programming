(ns mmp.task3.task-3-1)

(defn heavy-even? [x] (do (Thread/sleep 10) (even? x)))

(defn split-seq
  [coll slice-sizes]
  (let [slice-size (first slice-sizes)
        split (split-at slice-size coll)]
    (lazy-seq (cons (first split) (split-seq (second split) (rest slice-sizes))))))

(defn split
  [slice-count coll]
  (let [size (count coll)
        slice-size (quot size slice-count)
        remainder (rem size slice-count)]
    (->> (range)
         (map #(if (< % remainder)
                 (inc slice-size)
                 slice-size))
         (split-seq coll)
         (take slice-count))))

(defn pfilter
  [pred threads-num coll]
  (->> (split threads-num coll)
       (map #(future (doall (filter pred %))))
       (doall)
       (mapcat deref)))

(defn -main
  []
  (let [coll (take 100 (iterate inc 0))]
    (println "Standard filter:")
    (time (println (doall (filter heavy-even? coll))))
    (println "Parallel filter:")
    (time (println (doall (pfilter heavy-even? 20 coll))))
    (shutdown-agents)))
