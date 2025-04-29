(ns app.macro)

(defmacro create-a-function
  [l]
  `(defn ~l [o#] (inc o#)))
