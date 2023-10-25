(ns mmp.task1.task-1-4)

(defn add-char-to-string [alphabet permutation]
  (map #(str permutation %) (filter #(not (= (last permutation) (get % 0))) alphabet))
  )

(defn add-char-to-strings [alphabet permutations]
  (reduce concat (map #(add-char-to-string alphabet %) permutations))
  )

(defn create-string-list [alphabet n]
  (if (and (not (= (count alphabet) 0)) (not (< n 0)))
    (nth (iterate #(add-char-to-strings alphabet %) alphabet) (dec n))
    '())
  )
