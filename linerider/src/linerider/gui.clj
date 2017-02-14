(ns linerider.gui
  (:import
    (javax.swing JFrame JPanel JButton Timer)
    (java.awt Dimension Color FlowLayout BasicStroke MouseInfo PointerInfo)
    (java.awt.event ActionListener MouseListener MouseMotionListener)
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
   :p2 [x2 y2]})

(defn newState []
  {:mode :line
   :offset [0 0]
   :lines (list)})

;Timer that sends update signals to the frame to repaint
(defn updateTimer [world]
 (proxy [ActionListener] []
   (actionPerformed [e]
       (.revalidate world)
       (.repaint world)
   )))

(defn handleDrawLineDrag [e drawingState offset]
  (let [[offX offY] offset
        [baseSX baseSY] (@drawingState :p1)
        mX (+ (.getX e) offX)
        mY (+ (.getY e) offY)
        sX (+ baseSX offX)
        sY (+ baseSY offY)]
    (dosync
      (ref-set drawingState (assoc @drawingState :p2 [mX mY])))))

(defn handleMoveDrag [e dragState worldState]
  (let [[cOffX cOffY] (@worldState :offset)
        newOffX (+ (.getX e) cOffX)
        newOffY (+ (.getY e) cOffY)]
    (dosync (ref-set dragState (assoc @dragState :p2 [newOffX newOffY])))))


(defn dragListener [drawingState dragState worldState]
  (proxy [MouseMotionListener] []

    (mouseDragged [e]
      (if (= (@worldState :mode) :line)
        (handleDrawLineDrag e drawingState (@worldState :offset))
        (handleMoveDrag e dragState worldState)))

    (mouseMoved [e])))

(defn drawListener [drawingState worldState]
  (proxy [MouseListener] []

    (mousePressed [e]
      (if (= (@worldState :mode) :line)
        (let [[offX offY] (@worldState :offset)
              x (+ (.getX e) offX)
              y (+ (.getY e) offY)]
          (dosync
            (ref-set drawingState (assoc @drawingState :currentlyDrawing true))
            (ref-set drawingState (assoc @drawingState :p1 [x y]))
            (ref-set drawingState (assoc @drawingState :p2 [(+ x 1) (+ y 1)]))))))

    (mouseReleased [e]
      (if (= (@worldState :mode) :line)
        (let [[offX offY] (@worldState :offset)
              [baseSX baseSY] (@drawingState :p1)
              startX (+ baseSX offX)
              startY (+ baseSY offY)
              endX (+ (.getX e) offX)
              endY (+ (.getY e) offY)]
          (dosync
            (ref-set drawingState (assoc @drawingState :currentlyDrawing false))
            (ref-set drawingState (assoc @drawingState :p2 [endX endY]))
            (ref-set worldState (assoc @worldState :lines (cons (newLine startX startY endX endY) (@worldState :lines))))))))

    (mouseEntered [e])

    (mouseExited [e])

    (mouseClicked [e])))

(defn dragClickListener [dragState worldState]
  (proxy [MouseListener] []

    (mousePressed [e]
      (if (= (@worldState :mode) :drag)
        (let [[offX offY] (@worldState :offset)
              x (+ (.getX e) offX)
              y (+ (.getY e) offY)]
          (dosync
            (ref-set dragState (assoc @dragState :currentlyDragging true))
            (ref-set dragState (assoc @dragState :p1 [x y]))
            (ref-set dragState (assoc @dragState :p2 [x y]))))))

    (mouseReleased [e]
      (if (= (@worldState :mode) :drag)
        (let [[baseOffX baseOffY] (@worldState :offset)
              [sX sY] (@dragState :p1)
              [eX eY] (@dragState :p2)
              newXDif (- eX sX)
              newYDif (- eY sY)]
          (dosync
            (ref-set dragState (assoc @dragState :currentlyDragging false))
            (ref-set dragState (assoc @dragState :p1 [0 0]))
            (ref-set dragState (assoc @dragState :p2 [0 0]))
            (ref-set worldState (assoc @worldState :offset [(+ baseOffX newXDif) (+ baseOffY newYDif)]))))))

    (mouseEntered [e])

    (mouseExited [e])

    (mouseClicked [e])))

;;;;;;;;;;;;;;;;;;;;;;;
;Function Model
;;;;;;;;;;;;;;;;;;;;;;;

(defn doNothing []
 (proxy [ActionListener] []
   (actionPerformed [e]
     (println "nothing"))))

(defn modeSet [mode state]
  (proxy [ActionListener] []
    (actionPerformed [e]
      (dosync (ref-set state (assoc @state :mode mode))))))

(defn paint [g line offset dragState]
  (let [[offX offY] offset
        [sCOffX sCOffY] (dragState :p1)
        [eCOffX eCOffY] (dragState :p2)
        cOffX (- eCOffX sCOffX)
        cOffY (- eCOffY sCOffY)
        fullOffX (+ offX cOffX)
        fullOffY (+ offY cOffY)
        [sX sY] (line :p1)
        [eX eY] (line :p2)
        toDraw (new java.awt.geom.Line2D$Double (+ sX  fullOffX) (+ sY fullOffY) (+ eX fullOffX) (+ eY fullOffY))]
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

  (def drawingState
    (ref {:currentlyDrawing false
          :p1 nil
          :swingObj nil}))

  (def dragState
    (ref {:currentlyDragging false
          :p1 [0 0]
          :p2 [0 0]}))

  (let [p (proxy [JPanel] []
            (getPreferredSize []
              WORLDBOUNDS)

            (paintComponent [g]
              (proxy-super paintComponent g)

              (if (@drawingState :currentlyDrawing)
                    (paint g @drawingState (@state :offset) @dragState))

              (loop [lines (@state :lines)]
                  (if (empty? lines)
                    :done
                    (do
                      (paint g (first lines) (@state :offset) @dragState)
                      (recur (rest lines)))))))]

    (.addMouseListener p (drawListener drawingState state))
    (.addMouseListener p (dragClickListener dragState state))
    (.addMouseMotionListener p (dragListener drawingState dragState state))
    (.setLayout p (FlowLayout.))
    (.setBackground p Color/white)
    p))


(defn createButton [label listener]
  (let [b (JButton.)]
    (.addActionListener b listener)
    (.setText b label)
    b))

(defn createWorld []
  ;create our state
  (def state (ref (newState)))

  ;create our major gui objects
  (def controls (controlPanel))
  (def world (worldPanel state))
  (def frame (gameFrame))

  ;add buttons to the control panel
  (.add controls (createButton "play" (doNothing)))
  (.add controls (createButton "line" (modeSet :line state)))
  (.add controls (createButton "drag" (modeSet :drag state)))

  (.add frame world)
  (.add frame controls)

  (.start (Timer. UPDATERATE (updateTimer world)))

  (.pack frame)
  (.setVisible frame true)
  (.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE))
