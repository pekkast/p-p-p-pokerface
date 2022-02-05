(ns p-p-p-pokerface)

(defn rank [[rank]]
  (if (Character/isDigit rank)
    (Integer/valueOf (str rank))
    (let [ranks {\T 10
                 \J 11
                 \Q 12
                 \K 13
                 \A 14}]
      (get ranks rank))))

(defn suit [[_ suit]]
  (str suit))

(defn rank-frequencies [hand]
  (vals (frequencies (map rank hand))))

(defn same-at-least? [num hand]
  (not (empty? (filter #(>= % num)
                       (rank-frequencies hand)))))

(defn pair? [hand]
  (same-at-least? 2 hand))

(defn three-of-a-kind? [hand]
  (same-at-least? 3 hand))

(defn four-of-a-kind? [hand]
  (same-at-least? 4 hand))

(defn flush? [hand]
  (== 1 (count (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= '(3 2) (rank-frequencies hand)))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand) (= '(2 2 1) (rank-frequencies hand))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        min (apply min ranks)
        max (apply max ranks)
        compare (if (and (= 14 max) (= 2 min))
                  (sort (conj (range 2 6) 14))
                  (range min (+ min 5)))]
    (= compare ranks)))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map (fn [[check val]] (if (check hand) val 0)) checkers))))
