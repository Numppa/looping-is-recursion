(ns looping-is-recursion)

(defn power [base exp]
  (let
    [helper (fn [x base2 exp2]
              (cond (zero? exp2) 1
                    (== 1 exp2) base2
                    :else (recur x (* base2 x) (dec exp2))))]
    (helper base base exp)))

(defn last-element [a-seq]
  (let
    [helper (fn [b-seq]
              (cond (empty? b-seq) nil
               (== 1 (count b-seq)) (first b-seq)
                :else (recur (rest b-seq))))]
      (helper a-seq)))

(defn seq= [seq1 seq2]
  (let
    [helper (fn [seq1 seq2]
              (cond
               (and (empty? seq1) (empty? seq2)) true
               (or (empty? seq1) (empty? seq2)) false
               (not (== (first seq1) (first seq2))) false
               :else (recur (rest seq1) (rest seq2))))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [pred pred
         n 0
         a-seq a-seq]
    (cond (empty? a-seq) nil
          (pred (first a-seq)) n
          :else (recur pred (inc n) (rest a-seq)))))

(defn avg [a-seq]
  (loop
    [sum 0
     n 0
     a-seq a-seq]
    (if
      (empty? a-seq) (if (zero? n)
                       0
                       (/ sum n))
      (recur (+ sum (first a-seq)) (inc n) (rest a-seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop
    [a-seq a-seq
     a-set #{}]
    (if (empty? a-seq) a-set
    (recur (rest a-seq) (toggle a-set (first a-seq))))))

(defn fast-fibo [n]
  (loop
    [n n
     f1 0
     f2 1]
    (if (zero? n) (min f1 f2)
    (recur (dec n) (+ f1 f2) (max f1 f2)))))

(defn cut-at-repetition [a-seq]
  (loop
    [a-seq a-seq
     b-seq []]
    (if (or (contains? (set b-seq) (first a-seq)) (empty? a-seq)) b-seq
      (recur (rest a-seq) (conj b-seq (first a-seq))))))
