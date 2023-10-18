(ns mmp.task1.task-1-4)

(defn add-char-to-string [chars string]
  (map #(str string %) (filter #(not (= (last string) (get % 0))) chars)))

(defn add-char-to-strings [chars strings]
  (reduce concat (map #(add-char-to-string chars %) strings)))

(defn create-string-list [chars n]
  (if (and (not (= (count chars) 0)) (not (< n 0)))
    (nth (iterate #(add-char-to-strings chars %) chars) (dec n))
    '()))
