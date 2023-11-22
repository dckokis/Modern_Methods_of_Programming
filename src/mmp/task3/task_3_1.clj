(ns mmp.task3.task-3-1)

(defn heavy-even? [x] (do (Thread/sleep 10) (even? x)))

(defn split-seq
  [coll chunk-sizes]
  (let [chunk-size (first chunk-sizes)
        split-coll (split-at chunk-size coll)]              ; можно на take и drop разбить
    (lazy-seq
      (cons (first split-coll)
            (split-seq (second split-coll)
                       (rest chunk-sizes))))))

(defn _partition
  [chunk-count coll]
  (let [coll-size  (count coll)
        chunk-size (quot coll-size chunk-count)
        remainder  (rem coll-size chunk-count)]
    (->> (range)
         (map
           #(if (< % remainder)
              (inc chunk-size)
              chunk-size))
         (split-seq coll)
         (take chunk-count))))

(defn parallel-filter
  [pred threads-num coll]
  (->> (_partition threads-num coll)
       (map #(future (doall (filter pred %))))
       (doall)
       (mapcat deref)))

(defn -main
  []
  (let [coll (take 100 (iterate inc 0))]
    (println "Sequence:")
    (println coll)
    (println "Standard filter:")
    (time (println (doall (filter heavy-even? coll))))
    (println "Parallel filter:")
    (time (println (doall (parallel-filter heavy-even? 10 coll))))
    (shutdown-agents)))
