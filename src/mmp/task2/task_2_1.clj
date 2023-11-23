(ns mmp.task2.task-2-1)

(defn trapezia [f t_1 t_2]
  (* (- t_1 t_2) (/ (+ (f t_1) (f t_2)) 2)))

;Not memoized---------------------------------
(defn integral [f step x]
  (if (< 0 x)
    (+ (integral f step (- x step)) (trapezia f x (- x step)))
    0))

;Memoized-------------------------------------
(def integral-mem
  (memoize
    (fn [f step x]
      (if (< 0 x)
        (+ (integral-mem f step (- x step)) (trapezia f x (- x step)))
        0))))

;For comparison-------------------------------
(defn part-integral [function step]
  (partial integral function step))

(defn part-integral-mem [function step]
  (partial integral-mem function step))

;Comparison-----------------------------------
(defn -main
  []
  (let [integrate-memoized (part-integral-mem #(do (Thread/sleep 1) (* 4 %)) 1)
        integrate-not-memoized (part-integral #(do (Thread/sleep 1) (* 4 %)) 1)]
    (println "Not memoized")
    (time (integrate-not-memoized 500))
    (time (integrate-not-memoized 400))
    (time (integrate-not-memoized 500))
    (time (integrate-not-memoized 500))
    (println "Memoized")
    (time (integrate-memoized 500))
    (time (integrate-memoized 500.1))
    (time (integrate-memoized 500))
    (time (integrate-memoized 500))))