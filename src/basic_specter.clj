(ns basic-specter)

(defprotocol StructurePath
  (select* [this structure next-fn])
  (transform* [this structure next-fn]))

(defn combine-two-structure-path [path1 path2]
  (reify StructurePath
    (select* [this structure next-fn]
      (select* path1
               structure
               (fn [structure2]
                 (select* path2 structure2 next-fn)
                 ))
      )
    (transform* [this structure next-fn]
      (transform* path1
                  structure
                  (fn [structure2]
                    (transform* path2 structure2 next-fn)))
      )))

(defn comp-paths [path]
  (reduce combine-two-structure-path path))

(defn select [path structure]
  (select* (comp-paths path) structure (fn [v] [v])))

(defn transform [path transform-fn structure]
  (transform* (comp-paths path) structure transform-fn))


;; Implementation of keyword, ALL, and filter navigators

(extend-protocol StructurePath
  clojure.lang.Keyword
  (select* [kw structure next-fn]
    (next-fn (get structure kw)))
  (transform* [kw structure next-fn]
    (assoc structure kw (next-fn (get structure kw))))
  )

(deftype AllPath [])

(def ALL (->AllPath))

(extend-protocol StructurePath
  AllPath
  (select* [this structure next-fn]
    (mapcat next-fn structure))
  (transform* [this structure next-fn]
    (let [ret (map next-fn structure)]
      (if (vector? structure)
        (vec ret)
        ret)
      ))
  )

(extend-protocol StructurePath
  clojure.lang.AFn
  (select* [afn structure next-fn]
    (if (afn structure) (next-fn structure))
    )
  (transform* [afn structure next-fn]
    (if (afn structure) (next-fn structure) structure)
    )
  )
