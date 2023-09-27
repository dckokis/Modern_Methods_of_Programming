(ns mmp.task1.task-1-2)

(defn add-char-to-string
  ([alphabet current-permutation res]
   (if (> (count alphabet) 0)
     (if (= (first alphabet) (last current-permutation))
       (recur (rest alphabet) current-permutation res)
       (recur (rest alphabet) current-permutation (concat res (list (str current-permutation (first alphabet))))))
     res))
  ([alphabet current-permutation] (add-char-to-string alphabet current-permutation ()))
  )

(defn add-char-to-strings
  ([alphabet permutations res]
   (if (> (count permutations) 0)
     (recur
       alphabet (rest permutations) (concat (add-char-to-string alphabet (first permutations) ()) res))
     res))

  ([alphabet permutations]
   (add-char-to-strings alphabet permutations ()))
  )


(defn create-string-list
  ([alphabet num res]
   (if (> num 0)
     (recur alphabet (- num 1) (add-char-to-strings alphabet res ()))
     res))
  ([alphabet num]
   (create-string-list alphabet num (list "")))
  )

(defn main-1-2
  []
  (println (create-string-list '(\a, \b, \c) 3)))

(main-1-2)