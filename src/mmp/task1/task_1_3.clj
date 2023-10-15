(ns mmp.task1.task-1-3)

(defn my-map
  ([func coll]
   (reverse
     (reduce
       (fn [lst x] (cons (func x) lst))
       (list)
       coll))))
(defn my-filter
  ([pred coll]
   (reverse
     (reduce
       (fn [lst x]
         (if (pred x) (cons x lst) lst))
       (list)
       coll))))
