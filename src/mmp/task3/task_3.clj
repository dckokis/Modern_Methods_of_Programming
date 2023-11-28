(ns mmp.task3.task-3)

(defn heavy-even? [x] (do (Thread/sleep 10) (even? x)))

(defn split-seq
  [coll slice-sizes]
  (let [slice-size (first slice-sizes)
        split (split-at slice-size coll)]
    (lazy-seq (cons (first split) (split-seq (second split) (rest slice-sizes))))))

(defn slice
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
  (->> (slice threads-num coll)
       (map #(future (doall (filter pred %))))
       (doall)
       (mapcat deref)))

(defn lazy-pfilter
  [pred step n coll]
  (lazy-seq (concat (pfilter pred n (take step coll))
                    (when (seq coll)
                      (lazy-pfilter pred step n (drop step coll))))))

(defn -main
  []
  (let [coll (take 100 (iterate inc 0))]
    (println "Standard filter:")
    (time (println (filter heavy-even? coll)))
    (println "Parallel filter:")
    (time (println (pfilter heavy-even? 10 coll)))
    (println "Lazy parallel filter:")
    (time (println (lazy-pfilter heavy-even? 10 10 coll)))
    (shutdown-agents)))
