(ns ticktickticktock.core
  (:require [potemkin :refer [def-map-type]]))

(defn ^:private get<= [sm k]
  (first (rsubseq sm <= k)))

(defn ^:private get>= [sm k]
  (first (subseq sm >= k)))

(defn ^:private rightmost [sm]
  (first (rseq sm)))

(defn ^:private second-rightmost [sm]
  (second (rseq sm)))

(def-map-type TTTTMap [^clojure.lang.Sorted sm]

  (get [this k not-found]
    (if (neg? (.compare (.comparator sm) (key (rightmost sm)) k))
      nil
      (if-let [e (get<= sm k)]
        (val e)
        not-found)))

  (assoc [this k v]
    (if (seq sm)
      (let [[rk rv] (rightmost sm)
            comp    (.compare (.comparator sm) rk k)]
        (if (neg? comp)
          (TTTTMap. (-> sm
                        (cond-> (and (= rv v)
                                     (second-rightmost sm)
                                     (= (val (second-rightmost sm)) v))
                          (dissoc rk))
                        (assoc k v)))
          (throw
            (ex-info
              "This is a prototype, assoc supported only past the right end."
              {}))))
      (TTTTMap. (assoc sm k v))))

  (dissoc [this k]
    (assert false
      "This is a prototype, dissoc not supported."))

  (keys [this]
    (keys sm))

  (meta [this]
    (meta sm))

  (with-meta [this meta]
    (TTTTMap. (with-meta sm meta))))
