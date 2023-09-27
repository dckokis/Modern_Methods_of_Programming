(ns mmp.task1.task-1-1)

(defn add-char-to-string
  ([alphabet current-permutation res]
   (if (> (count alphabet) 0)
     (if (= (first alphabet) (last current-permutation))
       (add-char-to-string (rest alphabet) current-permutation res)
       (add-char-to-string (rest alphabet) current-permutation (concat res (list (str current-permutation (first alphabet))))))
     res))
  )

(defn add-char-to-strings
  ([alphabet permutations res]
   (if (> (count permutations) 0)
     (add-char-to-strings
       alphabet (rest permutations) (concat (add-char-to-string alphabet (first permutations) ()) res))
     res))
  )


(defn create-string-list
  ([alphabet num res]
   (if (> num 0)
     (create-string-list alphabet (- num 1) (add-char-to-strings alphabet res ()))
     res))
  )

(defn main-1-1
  []
  (println (create-string-list '(\a, \b, \c) 3 (list ""))))

(main-1-1)