(ns mmp.task2.task-2-1)

(defn trapezia-area [f t_1 t_2]
  (* (- t_1 t_2) (/ (+ (f t_1) (f t_2)) 2))
  )

;Not memoized---------------------------------
(defn integral [f step x]
  (if (< 0 x)
    (+ (integral f step (- x step)) (trapezia-area f x (- x step)))
    0)
  )

;Memoized-------------------------------------
(def integral-memoized (memoize integral))
(def trapezia-memoized (memoize trapezia-area))

(defn integral-mem [f step x]
  (if (< 0 x)
    (+ (integral-memoized f step (- x step)) (trapezia-memoized f x (- x step)))
    0)
  )

;For comparison-------------------------------
(defn part-integral [function step]
  (partial integral function step)
  )

(defn part-integral-mem [function step]
  (partial integral-mem function step)
  )

;Comparison-----------------------------------
(defn -main
  []
  (let [integrate-memoized (part-integral-mem #(do (Thread/sleep 1) (* -4 %)) 0.01)
        integrate-not-memoized (part-integral #(do (Thread/sleep 1) (* -4 %)) 0.01)]
    (println "Not memoized")
    (time (integrate-not-memoized 50))
    (time (integrate-not-memoized 50))
    (time (integrate-not-memoized 50))
    (time (integrate-not-memoized 50))
    (println "Memoized")
    (time (integrate-memoized 50))
    (time (integrate-memoized 50))
    (time (integrate-memoized 50))
    (time (integrate-memoized 50))))