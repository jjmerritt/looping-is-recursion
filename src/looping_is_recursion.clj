(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc e]
                 (if (zero? e)
                   acc
                   (recur (* acc base) (dec e))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (cond
    (empty? a-seq)
      nil
    (empty? (rest a-seq))
      (first a-seq)
    :else
      (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2))
      true
    (or (empty? seq1) (empty? seq2))
      false
    (= (first seq1) (first seq2))
      (recur (rest seq1) (rest seq2))
    :else
      false))

(defn find-first-index [pred a-seq]
  (loop [acc 0
         i-seq a-seq]
    (cond
      (empty? i-seq)
        nil
      (pred (first i-seq))
        acc
      :else
        (recur (inc acc) (rest i-seq)))))

(defn avg [a-seq]
  (let [total (max 1 (count a-seq))]
    (loop [acc 0
           i-seq a-seq]
      (if (empty? i-seq)
        (/ acc total)
        (recur (+ acc (first i-seq)) (rest i-seq))))))

(defn toggle [a-set elem] 
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [a-set #{}
         i-seq a-seq]
    (if (empty? i-seq)
      a-set
      (recur (toggle a-set (first i-seq)) (rest i-seq)))))

(defn fast-fibo [n]
  (if (= 0 n)
    0
    (loop [loopn n
           current 1
           n-1 0]
      (if (= 1 loopn)
        current
        (recur (dec loopn) (+ current n-1) current)))))

(defn cut-at-repetition [a-seq]
  (loop [cut '[]
         cut-set #{}
         i-seq a-seq]
    (cond
      (empty? i-seq)
        cut
      (contains? cut-set (first i-seq))
        cut
      :else
        (recur 
          (conj cut (first i-seq)) 
          (conj cut-set (first i-seq)) 
          (rest i-seq)))))

