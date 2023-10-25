(ns mmp.task2.task-2-1)

(defn trapezia-area [f t_1 t_2]
  (* (- t_1 t_2) (/ (+ (f t_1) (f t_2)) 2))
  )

;Not memoized---------------------------------
(defn integral [f step x]
  (if (< 0 x)
    (+ (integral f step (- x step)) (trapezia-area f (- x step) x))
    0)
  )

(defn part-integral [function step]
  (partial integral function step)
  )

;Memoized-------------------------------------
(defn integral-mem [function step]
  (let [mem-integrate (memoize integral)]
    (partial mem-integrate function step))
  )

;Comparison-----------------------------------
(defn -main
  []
  (let [integrate-mem (integral-mem #(* -4 %) 0.01)
        integrate-base (part-integral #(* -4 %) 0.01)]
    (println "Not memoized")
    (time (integrate-base 50))
    (time (integrate-base 50))
    (time (integrate-base 50))
    (time (integrate-base 50))
    (println "Memoized")
    (time (integrate-mem 50))
    (time (integrate-mem 50))
    (time (integrate-mem 50))
    (time (integrate-mem 50))))