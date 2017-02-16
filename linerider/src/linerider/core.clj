; Author1: Benjamin Efron
; Author2: Justin Willoughby
; Date: Winter 2016/17
; Course: CSSE403 - Programming Language Paradigms
;
; Module Description: Main runner for our linerider project

(ns linerider.core
  (:require linerider.gui))

(defn -main [] (linerider.gui/createWorld))
