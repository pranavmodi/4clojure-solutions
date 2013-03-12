(ns forclojure
  (:use [clojure.set]))

(fn [n c]
  (list (take n c) (drop n c)))

(defn remd
  [c]
  (if (< (count c) 2)
    c
    (if (= (first c) (nth c 1))
     (remd (rest c))
     (cons (first c) (remd (rest c))))))

(defn inter
  ([s1 s2]
     (inter s1 s2 '()))
  ([s1 s2 c]
     (if (or (= nil (first s1)) (= nil (first s2)))
       c
       (cons (first s1) (cons (first s2) (inter (rest s1) (rest s2)))))))

(defn replic
  [s n]
  (mapcat (partial repeat n) s))

#(mapcat (partial repeat %2) %1)

(defn interp
  [c e]
  (butlast (mapcat #(list % e) c)))

#(partition-by identity %)

(defn rem-c
  [c n]
  (if (< (count c) n)
    c
    (concat (take (dec n) c) (rem-c (drop n c) n))))

(defn h-tr
  [& b]
  (let [cb (reduce #(if %2 (inc %1) %1) 0 b)]
    (println cb)
    (println (count b))
    (and (< cb (count b)) (> cb 0))))

(defn cart-p
  [s1 s2]
  (apply hash-set (apply concat (map (fn [b] (map #(vector b %) s2)) s1))))

(defn dig-p
  [a b]
  (map (comp #(Integer/parseInt %) str) (str (* a b))))

(defn my-group
  [f c]
  (reduce #(assoc %1 (first %2) (conj (vec (%1 (first %2))) (second %2))) {} (partition 2 (interleave (map f c) c))))

(defn sym-diff
  [s1 s2]
  (set (concat (filter #(not (contains? s2 %)) s1) (filter #(not (contains? s1 %)) s2))))

(defn fib
  [n]
  (if (= n 2)
    [1 1]
    (let [p (fib (- n 1))]
      (conj p (+ (last p) (last (butlast p)))))))

(defn rev
  [s]
  (if (= 2 (count s))
    (str (last s) (first s))
    (str (rev (rest s)) (first s))))

(defn print-table
  [n]
  (doseq [i (range 1 n)]
    (println "")
    (doseq [e (range 1 10)]
      (print (* i e) " "))))

(defn largest
  [s]
  (loop [c 0
         p s]
    (if (not= '() p)
      (if (> (first p) c)
       (recur (first p) (rest p))
       (recur c (rest p)))
      c)))

(defn my-clos
  [n]
  #(int (Math/pow % n)))

(defn no-of
  [s]
  (count
   (filter
    (fn [n] (< n (reduce + 0 (map #(Math/pow (- (int %) 48) 2) (str n))))) s)))

(defn tree?
  [t]
  (if (= nil t)
    true
    (if t
      (and (= 3 (count t)) (tree? (second t)) (tree? (last t)))
      false)))

(defn pascals
  [n]
  (loop [i n
         v [1]]
    (if (= i 1)
      v
      (recur (dec i) ((fn pas
                        [vn t]
                        (if (= (count vn) 1)
                          (conj t 1)
                          (pas (rest vn) (conj t (+ (first vn) (second vn)))))) v [1])))))

(defn dot
  [s1 s2]
  (reduce + 0 (map * s1 s2)))

(defn dis
  [c]
  (loop [t [(first c)]
         s (rest c)]
    (if (empty? s)
      t
      (recur (if (contains? (set t) (first s)) t (conj t (first s))) (rest s)))))

(defn my-diff
  [s1 s2]
  (let [set1 (set s1)
        set2 (set s2)]
    (vec (clojure.set/difference (clojure.set/union set1 set2) (clojure.set/intersection set1 set2)))))

(defn get-p
  [n]
  (loop [i 3
         s [2]]
    (if (= (count s) n)
      s
      (recur (+ i 2)
             (if (> (count (filter #(= 0 (mod i %)) (range 2 (inc (/ i 2))))) 0)
               s
               (conj s i))))))

(def sor-set (sorted-set 1 2 3 4 5))

(defn rev
  [coll]
  (if (= [] coll)
    []
    (conj (rev (rest coll)) (first coll))))

(defn pal
  [coll]
  (if (< (count coll) 2)
    true
    (and (= (first coll) (last coll)) (pal (drop 1 (take (- (count coll) 1) coll))))))

(defn mk
  [& coll]
  (reduce #(if (< %1 %2) %2 %1) coll))

(defn fil-str [line]
  (apply str (filter #(Character/isUpperCase %) (seq line))))

(defn ran
  [r1 r2]
  (if (>= r1 r2)
    '()
    (cons r1 (ran (inc r1) r2))))

(fn [x]
  (filter (complement sequential?)
	  (rest (tree-seq sequential? seq x))))

(defn rotate-coll
  [n coll]
  (if (= n 0)
    coll
    (if (> n 0)
      (rotate-coll (dec n) (conj (vec (rest coll)) (first coll)))
      (rotate-coll (inc n) (cons (last coll) (vec (butlast coll)))))))

(fn [n s]
  (take (count s) (drop (mod n (count s)) (cycle s))))

(fn [f]
  (fn [& args] (apply f (reverse args))))

(defn split-by-type
  ([coll]
     (split-by-type coll '()))
  ([coll split-coll]
     (if (= '() coll)
       split-coll
       (let [filter-by-class #(= (class %) (class (first coll)))
	    class-coll (filter filter-by-class coll)
	    rest-coll (remove filter-by-class coll)]
	 (split-by-type rest-coll (cons class-coll split-coll))))))

(defn freq
  ([coll]
     (freq coll {}))
  ([coll f-map]
     (if (= '() coll)
       f-map
       (let [m-key (first coll)
	     val (f-map m-key)
	     occ (if (= nil val) 1 (inc val))]
	 (freq (rest coll) (assoc f-map m-key occ))))))

(defn gcd
  [a b]
  (if (= 0 (mod a b))
    b
    (gcd b (mod a b))))

(defn inter
  ([a b]
     (inter a b #{}))
  ([a b s]
     (if (empty? a)
       s
       (if (contains? b (first a)) (inter (disj a (first a)) b (conj s (first a))) (inter (disj a (first a)) b s)))))


(defn square
  [x]
  (* x x))

(defn mapd
  [v k]
  (reduce #(assoc %1 %2 v) {} k))

(defn map-in
  [s]
  (map-indexed (fn [i x] [x i]) s))

(defn remap
  [f s]
  (if (empty? s)
    s
    (cons (f (first s)) (lazy-seq (remap f (rest s))))))

(defn seq-trial
  []
  (map #(do (println "hello " %) (inc %)) (range 100)))

(defn ints-from [n]
  (println "hello " n)
  (cons n (lazy-seq (ints-from (inc n)))))

(defn read-binary
  [n]
  (let [t (map-indexed (fn [i v] [i v]) (map (fn [c] (- (int c) 48)) (reverse n)))]
    (int (reduce (fn [s [i v]] (+ s (* v (Math/pow 2 i)))) 0 t))))

(defn setq
  [s]
  (= (count (apply union s)) (apply + (map count s))))

(defn happy
  [n]
  (letfn [(c [m] (- (int m) 48))
          (f [h] (apply + (map #(* (c %) (c %)) (str h))))]
    (loop [s #{}
           l (iterate f n)]
      (if (contains? s (first l))
        false
        (if (= 1 (first l))
          true
          (recur (conj s (first l)) (rest l)))))))


(defn idkv
  [s]
  (let [d (vec (partition-by #(if (keyword? %) % false) s))
        e (if (keyword? (last d))
            (conj d '())
            d)
        f (map #(if (keyword? (first %)) (first %) %) e)]
    (loop [g f
           h {}]
      (if (empty? g)
        h
        (if (and (keyword? (second g)) (keyword? (first g)))
          (recur (rest g) (assoc h (first g) '()))
          (recur (rest (rest g)) (assoc h (first g) (second g))))))))


(defn f [[x & s :as c]]
  (if (seq c)
   (let [[a b] (split-with number? s)]
     (merge {x a} (f b)))
   {}))

(comment filter #(> (count %) 1))

(defn anagram
  [s]
  (letfn [(z [w c] (count (filter #(= % true) (map #(= c %) w))))
          (m [a b] (reduce #(and %1 (= (z a %2) (z b %2))) true (distinct (concat a b))))]
    (set (distinct (filter #(> (count %) 1)
                       (loop [i (map #(set (vector %)) s)
                              w s]
                         (if (empty? w)
                           i
                           (recur (map #(if (and
                                             (m (first %) (first w))
                                             (not= (first %) (first w)))
                                          (conj % (first w))
                                          %) i)
                                  (rest w)))))))))

#(->> % (group-by sort) vals (filter next) (map set) set)

(defn camel
  [s]
  (let [a (clojure.string/split s #"-")
        b (apply str (map #(apply str (conj (drop 1 %) (char (- (int (first %)) 32)))) (rest a)))]
    (str (first a) b)))

(defn findn
  [& s]
  (loop [a (vec s)]
    (if (some nil? (map first a))
      nil
      (if (apply = (map first a))
        (first (map first a))
        (let [i (first (first (sort-by second (map-indexed (fn [j k] [j (first k)]) a))))]
         (recur (assoc a i (drop 1 (nth a i)))))))))