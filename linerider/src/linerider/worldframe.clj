(ns linerider.worldframe
  (:import
    (javax.swing JFrame JPanel JButton Timer)
    (java.awt Dimension Color FlowLayout BasicStroke)
    (java.awt.event ActionListener MouseListener)
    (java.awt.geom Ellipse2D Line2D))
  (:gen-class
    :prefix -
    :implements [JPanel]
    :state state
    :init init
    :methods [[drawLine]

(defn -init [worldState]
  [[] (ref worldState)])
