(ns p-p-p-pokerface)

(def face-rank {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[r] card]
    (if (Character/isDigit r) (Integer/valueOf (str r)) (Integer/valueOf (face-rank r)))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (let [freq (frequencies (vals (frequencies (map rank hand))))
        find-freq (fn [f] (find freq f))
        has-freq (fn [f many-times] (if (empty? (find-freq f)) false (= many-times ((find-freq f) 1))))]
    (or (has-freq 2 2) (has-freq 4 1))))

(defn straight? [hand]
  (let [get-rank-hand (fn [x] (sort (map rank x)))
        get-rank-low-ace-hand (fn [x] (sort (replace {14 1} (map rank x))))
        get-min-rank (fn [x] (apply min x))
        get-range (fn [x] (range (get-min-rank x) (+ (get-min-rank x) 5)))]
    (or (= (get-rank-hand hand) (get-range (get-rank-hand hand))) (= (get-rank-low-ace-hand hand) (get-range (get-rank-low-ace-hand hand))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        call-f (fn [f] (if ((f 0) hand) (f 1)))]
    (apply max (filter some? (map call-f checkers)))))
