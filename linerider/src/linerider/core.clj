(ns linerider.core
  (:require linerider.test))

(declare foo)

(defn -main [] (println (linerider.test/testAdd 1 2)))
  

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
