(ns linerider.gui
  (:import
    (javax.swing JFrame JPanel JButton Timer)
    (java.awt Dimension Color FlowLayout BasicStroke)
    (java.awt.event ActionListener)
    (java.awt.geom Ellipse2D Line2D)))

(declare doNothing)

;;;;;;;;;;;;;;;;;;;;;;;
;GLOBALS
;;;;;;;;;;;;;;;;;;;;;;;

(def FRAMEBOUNDS (Dimension. 1240 850))
(def WORLDBOUNDS (Dimension. 1200 740))
(def CONTROLBOUNDS (Dimension. 1200 100))
(def FPS 24)
(def UPDATERATE (/ 1000 FPS))

;;;;;;;;;;;;;;;;;;;;;;;
;MUTABLE MODEL
;;;;;;;;;;;;;;;;;;;;;;;

(defn newLine [x1 y1 x2 y2]
  {:p1 [x1 y1]
   :p2 [x2 y2]
   :swingObj (new java.awt.geom.Line2D$Double x1 y1 x2 y2)})

(defn newState []
  {:mode :draw
   :lines (list)
   :drawingstyle :line})

;Timer that sends update signals to the frame to repaint
(defn updateTimer [world]
 (proxy [ActionListener] []
   (actionPerformed [e]
       (.revalidate world)
       (.repaint world)
   )))

;;;;;;;;;;;;;;;;;;;;;;;
;Function Model
;;;;;;;;;;;;;;;;;;;;;;;

(defn doNothing []
 (proxy [ActionListener] []
   (actionPerformed [e]
     (println "nothing"))))

(defn paint [g line]
  (let [toDraw (line :swingObj)]
    (.setColor g Color/black)
    (.setStroke g (BasicStroke. 4))
    (.fill g toDraw)
    (.draw g toDraw)))

;;;;;;;;;;;;;;;;;;;;;;;
;GUI
;;;;;;;;;;;;;;;;;;;;;;;

;creates the frame to load our panels into
(defn gameFrame []
  (let [f (proxy [JFrame] []
            (getPreferredSize []
              FRAMEBOUNDS))]
      (.setLayout f (FlowLayout.))
      f))

;creates a panel for buttons
(defn controlPanel []
  (let [p (proxy [JPanel] []
            (getPreferredSize []
              CONTROLBOUNDS))]
      (.setLayout p (FlowLayout.))
      p))

;creates the simulation panel - everything happens here
(defn worldPanel [state]
  (let [p (proxy [JPanel] []
            (getPreferredSize []
              WORLDBOUNDS)

            (paintComponent [g]
              (proxy-super paintComponent g)
              (loop [lines (@state :lines)]
                  (if (empty? lines)
                    :done
                    (do
                      (paint g (first lines))
                      (recur (rest lines)))))))]
    (.setLayout p (FlowLayout.))
    (.setBackground p Color/white)
    p))

(defn playButton []
  (def listener (doNothing))
  (let [b (JButton.)]
    (.addActionListener b listener)
    (.setText b "play")
    b))

(defn drawButton []
  (def listener (doNothing))
  (let [b (JButton.)]
    (.addActionListener b listener)
    (.setText b "draw")
    b))

(defn lineButton []
  (def listener (doNothing))
  (let [b (JButton.)]
    (.addActionListener b listener)
    (.setText b "line")
    b))


(defn createWorld []

  ;create our major gui objects
  (def controls (controlPanel))
  (def world (worldPanel (ref (newState))))
  (def frame (gameFrame))

  ;add buttons to the control panel
  (.add controls (playButton))
  (.add controls (drawButton))
  (.add controls (lineButton))

  (.add frame world)
  (.add frame controls)

  (.start (Timer. UPDATERATE (updateTimer world)))

  (.pack frame)
  (.setVisible frame true)
  (.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE))
