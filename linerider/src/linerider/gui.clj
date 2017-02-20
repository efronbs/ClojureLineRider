; Author1: Benjamin Efron
; Author2: Justin Willoughby
; Date: Winter 2016/17
; Course: CSSE403 - Programming Language Paradigms
;
; Module Description: Gui/Editor functionality for our linerider project

(ns linerider.gui
  (:import
    (javax.swing JFrame JPanel JButton Timer)
    (java.awt Dimension Color FlowLayout BasicStroke MouseInfo PointerInfo)
    (java.awt.event ActionListener MouseListener MouseMotionListener KeyListener)
    (java.awt.geom Ellipse2D Line2D)))

(declare doNothing)
(declare getDistanceToLine)
(declare dropNth)
(declare getAcceleration)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;GLOBALS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def FRAMEBOUNDS (Dimension. 1240 850))
(def WORLDBOUNDS (Dimension. 1200 740))
(def CONTROLBOUNDS (Dimension. 1200 100))
(def FPS 24)
(def UPDATERATE (/ 1000 FPS))
(def SNAPRATE 500)
(def ERASEDISTANCE 10) ;arbitrarily chosen
(def OBSTACLE_SIDE_LENGTH 20) ;arbitrarily chosen
(def GRAVITY 0.5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;MUTABLE MODEL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description: Create a new rider object
;
; Parameters: x - x cord
;             y - y cord
;             size - radius of ball
;
; Return: rider
(defn newRider []
  {:cords [100 100]
   :size 10
   :xVel 0
   :yVel 0
   :jumping false})

; Description: Creates a new line object
;
; Parameters: x1 - starting x cord
;             y1 - starting y cord
;             x2 - ending x cord
;             y2 - ending y cord
;
; Return: line
(defn newLine [x1 y1 x2 y2]
  (let [[x1 x2] (if (= x1 x2) [x1 (+ 1 x1)] [x1 x2])
        [rx1 ry1 rx2 ry2] (if (> (- x2 x1) 0)
                            [x1 y1 x2 y2]
                            [x2 y2 x1 y1])
         m  (/ (- ry2 ry1) (- rx2 rx1))
         b  (- ry1 (* (/ (- ry2 ry1) (- rx2 rx1)) rx1))
         vector (Math/sqrt (+ (Math/pow (- ry2 ry1) 2) (Math/pow (- rx2 rx1) 2)))
         angle (Math/atan m)
         vX (* vector (Math/cos angle))
         vY (* vector (Math/sin angle))]
    {:p1 [rx1 ry1]
     :p2 [rx2 ry2]
     :m   m
     :b   b
     :vector vector
     :angle angle
     :vX vX
     :vY vY}))

; Description: Creates a new object
;
; Parameters: x - center x coordinate
;             y - center y coordinate
;
; Return: map representing obstacle.
; currently it is just the center, but architecting it like this would make it really simple to add different types of obstacles in the future
;
(defn newObstacle [x y]
  (let [cornerX (- x (/ OBSTACLE_SIDE_LENGTH 2))
        cornerY (- y (/ OBSTACLE_SIDE_LENGTH 2))]
    {:center [x y]
     :drawCorner [cornerX cornerY]}))

; Description: Creates a new state opject
;
; Parameters: None
;
; Return: state
(defn newState []
  {:mode :line
   :offset [0 0]
   :lines (list)
   :obstacles (list)
   :rider (newRider)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Game physics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description: Gets the absolute value of the given number
;
; Parameters: n - number
;
; Return: absolute value of n
(defn abs [n]
  (if (< n 0)
    (- n)
    n))

; Description: Returns the reciprocal of the given number
;
; Parameters: n - number
;
; Return: reciprocal of n
(defn reciprocal [n]
  (if (= n 0)
    0
    (/ 1 n)))

; Description: Removes the nth element from a sequence
;
; Parameters: seq - sequence object
;             index - index to remove
;
; Return:
(defn dropNth [seq index]
  (concat (take index seq) (nthrest seq (inc index))))

; Description: Gets the distance from a point to a line
;
; Parameters: line - line to get distance to
;             point - point to get distance from
;
; Return: distance to line from point
(defn getDistanceToLine [line point]
  (let [[mX mY] point
        [x1 y1] (line :p1)
        [x2 y2] (line :p2)
        top (Math/abs (- (+ (- (* (- y2 y1) mX) (* (- x2 x1) mY)) (* x2 y1)) (* y2 x1)))
        bot (Math/sqrt (+ (Math/pow (- y2 y1) 2) (Math/pow (- x2 x1) 2)))]
    (/ top bot)))

; Description: Gets the distance from a point to a obstacle
;
; Parameters: obstacle - obstacle to get distance to
;             point - point to get distance from
;
; Return: distance to obstacle from point
(defn getDistanceToObstacle [obstacle point]
  (let [[x1 y1] point
        [x2 y2] (obstacle :center)]
      (Math/sqrt (+ (Math/pow (- y2 y1) 2) (Math/pow (- x2 x1) 2)))))

; Description: Applies gravity to the rider
;
; Parameters: rider - rider specified in world state
;
; Return: Updated rider
(defn applyGravity [rider]
(assoc rider :yVel (+ (rider :yVel) GRAVITY)))

; Description: Checks whether the rider is colliding with any lines
;
; Parameters: rider - rider specified in world state
;             lsLines - list of lines draw in the world, held in world state
;
; Return: List [bool, line] where bool is whether collision was found
;         and the where line is the offending line
(defn collidingWithLine [rider lsLines]
 (let [[xcord ycord] (rider :cords)]
   (loop [lines lsLines]
     (if (empty? lines)
       [false, {}]
       (let [currentLine (first lines)
             p1 (currentLine :p1)
             p2 (currentLine :p2)
             [x1 y1] p1
             [x2 y2] p2]
           (if (and (< (abs (- ycord (+ (* (currentLine :m) xcord) (currentLine :b)))) (rider :size))
                    (or (and (> xcord x1) (< xcord x2)) (and (< xcord x1) (> xcord x2))))
             [true, currentLine]
             (recur (rest lines))))))))

; Description: Checks whether the rider is colliding with any obstacles
;
; Parameters: rider - rider specified in world state
;             lsobstacles - list of obstacles draw in the world, held in world state
;
; Return: List [bool, obstacle] where bool is whether collision was found
;         and the where obstacle is the offending obstacle
(defn collidingWithObstacle [rider lsobstacles]
  (let [[xcord ycord] (rider :cords)]
    (loop [obstacles lsobstacles]
      (if (empty? obstacles)
        [false, {}]
        (let [currentObstacle (first obstacles)
              center (currentObstacle :center)
              [c1 c2] center]
            (if ( < (Math/sqrt (+ (Math/pow (- xcord c1) 2) (Math/pow (- ycord c2) 2))) (+ (rider :size) ( / OBSTACLE_SIDE_LENGTH 2)))
              [true, currentObstacle]
              (recur (rest obstacles))))))))

; Description: Makes the rider jump by increasing yVel
;
; Parameters: rider - rider specified in world state
;
; Return: Updated rider
(defn jump [rider]
  (if (not (rider :jumping))
    (assoc rider :yVel (- (rider :yVel) 8)
                 :jumping true)
    rider))

; Description: Gets the acceleration
;
; Parameters: line - line colliding with ball
;
; Return: [accelX accelY] updated acceleration list
(defn getAcceleration [line]
  (let [angle (line :angle)
        forceParallel (* GRAVITY (Math/sin angle))
        accelX (* forceParallel (Math/cos angle))
        accelY (* forceParallel (Math/sin angle))]
    [accelX accelY]))


; Description: Updates the riders velocity on collision with given line
;
; Parameters: rider - rider specified in world state
;             line - line that the rider is coliding with
;
; Return: Updated rider
(defn updateVelocityOnCollisionLine [rider line]
  (let [angle (line :angle)
        [accelX accelY] (getAcceleration line)
        riderXVel (rider :xVel)
        riderYVel (rider :yVel)
        vX (line :vX)
        vY (line :vY)
        directionOfVelocity (if (< (+ (* riderXVel vX) (* riderYVel vY)) 0) -1 1)
        riderVMag (* (Math/sqrt (+ (Math/pow riderXVel 2) (Math/pow riderYVel 2))) directionOfVelocity)
        rotatedRiderXVel (* riderVMag (Math/cos angle))
        rotatedRiderYVel (* riderVMag (Math/sin angle))
        finalRiderVelX (+ rotatedRiderXVel accelX)
        finalRiderVelY (+ rotatedRiderYVel accelY)]
    (assoc rider :xVel finalRiderVelX
                 :yVel  finalRiderVelY
                 :jumping false)))

; Description: Updates the riders velocity on collision with given obstacle
;
; Parameters: rider - rider specified in world state
;             obstacle - obstacle that the rider is coliding with
;
; Return: Updated rider
(defn updateVelocityOnCollisionObstacle [rider obstacle]
  (let [
        currentXVel (rider :xVel)
        currentYVel (rider :yVel)
        newXVel 0
        newYVel 0
       ]
     (do (println "Values -----------------------------------------")
         (println " NewXVel " newXVel " NewYVel " newYVel)
         (println)
       (assoc rider :xVel newXVel
                    :yVel newYVel
                    :jumping true))))

; Description: Handles collisions between the rider and the lines
;
; Parameters: state - world state
;
; Return: void
(defn handleCollisions [state]
 (let [rider (state :rider)
       lines (state :lines)
       obstacles (state :obstacles)
       collisionLine (collidingWithLine rider lines)
       collisionObstacle (collidingWithObstacle rider obstacles)
       [lineBool line] collisionLine
       [obstacleBool obstacle] collisionObstacle]
     (if obstacleBool
       (do
         (updateVelocityOnCollisionObstacle rider obstacle))
       (if lineBool
         (do
           (updateVelocityOnCollisionLine rider line))
         (do
           (applyGravity rider))))))

; Description: Updates the rider's x and y cords based on xvel and yvel
;              Also applies gravity to rider
;
; Parameters: rider - rider specified in the world state
;
; Return: void
(defn updateRider [rider]
 (let [[xcord ycord] (rider :cords)]
     (assoc rider :cords [(+ xcord (rider :xVel)) (+ ycord (rider :yVel))])))

; Description: Applies game physics to rider
;
; Parameters: None
;
; Return: void
(defn applyGamePhysics [state]
 (do
   (dosync (ref-set state (assoc @state :rider (updateRider (state :rider)))))
   (dosync (ref-set state (assoc @state :rider (handleCollisions state))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Listeners
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description: Dummy ActionListener
;
; Parameters: None
;
; Return: ActionListener
(defn doNothing []
 (proxy [ActionListener] []
   (actionPerformed [e]
     (println "nothing"))))

; Description: ActionListener to set the game mode
;
; Parameters: mode - mode to set the game to
;             state - world state
;
; Return: ActionListener
(defn modeSet [mode state]
  (proxy [ActionListener] []
    (actionPerformed [e]
      (do
        (if (= mode :play)
          (dosync (ref-set state (assoc @state :offset [100 100])))
          (dosync (ref-set state (assoc @state :offset [0 0]))))
        (dosync (ref-set state (assoc @state :mode mode)))))))

; Description: Timer that sends update signals to the frame to repaint
;
; Parameters: world - world panel
;
; Return: ActionListener
(defn updateTimer [world]
  (proxy [ActionListener] []
  (actionPerformed [e]
     (.revalidate world)
     (.repaint world)
  )))

  ; (defn newRider []
  ; {:cords [100 100]
  ;  :size 20
  ;  :xVel 0
  ;  :yVel 0
  ;  :jumping false})
;
; (defn snapRiderTimer [state]
;   (proxy [ActionListener] []
;     (actionPerformed [e]
;       (let [rider (state :rider)
;             [drawX drawY] (rider :cords)
;             []


; Description: Handles setting the drawing state
;
; Parameters: e - MouseMotionListener event
;             drawingState - current drawing state
;             offset - current offset; held in the world state
;
; Return: void
(defn handleDrawLineDrag [e drawingState offset]
  (let [[offX offY] offset
      mX (- (.getX e) offX)
      mY (- (.getY e) offY)]
    (dosync
      (ref-set drawingState (assoc @drawingState :p2 [mX mY])))))

; Description: Handles setting the drag state
;
; Parameters: e - MouseMotionListener event
;             dragState - current drag state
;             state - world state
;
; Return: void
(defn handleMoveDrag [e dragState state]
  (let [[cOffX cOffY] (@state :offset)
      newOffX (+ (.getX e) cOffX)
      newOffY (+ (.getY e) cOffY)]
    (dosync
      (ref-set dragState (assoc @dragState :p2 [newOffX newOffY])))))

; Description: Drag MouseMotionListener, determines what to do when the mouseDragged
;              is dragged. If the mode is line, then it should draw line,
;              else, it should drag the screen
;
; Parameters: drawingState - current drawing state
;             dragState - current drag state
;             state - world state
;
; Return: MouseMotionListener
(defn dragListener [drawingState dragState state]
  (proxy [MouseMotionListener] []

    (mouseDragged [e]
      (case (@state :mode)
        :line
          (handleDrawLineDrag e drawingState (@state :offset))
        :drag
          (handleMoveDrag e dragState state)
        "default"))

    (mouseMoved [e])))

; Description: Draw MouseListener, used to create lines in editor
;
; Parameters: drawingState - current drawing state
;             state - world state
;
; Return: MouseListener
(defn drawListener [drawingState state]
  (proxy [MouseListener] []

    (mousePressed [e]
      (if (= (@state :mode) :line)
        (let [[offX offY] (@state :offset)
              x (- (.getX e) offX)
              y (- (.getY e) offY)]
          (dosync
            (ref-set drawingState (assoc @drawingState :currentlyDrawing true))
            (ref-set drawingState (assoc @drawingState :p1 [x y]))
            (ref-set drawingState (assoc @drawingState :p2 [(+ x 1) (+ y 1)]))))))

    (mouseReleased [e]
      (if (= (@state :mode) :line)
        (let [[offX offY] (@state :offset)
              [sX sY] (@drawingState :p1)
              [eX eY] (@drawingState :p2)]
          (dosync
            (ref-set drawingState (assoc @drawingState :currentlyDrawing false))
            (ref-set state (assoc @state :lines (cons (newLine sX sY eX eY) (@state :lines))))))))

    (mouseEntered [e])

    (mouseExited [e])

    (mouseClicked [e])))

; Description: Drag MouseListener, used to handle drag when mouse
;              is pressed and released
;
; Parameters: dragState - current drag state
;             state - world state
;
; Return: MouseListener
(defn dragClickListener [dragState state]
  (proxy [MouseListener] []

    (mousePressed [e]
      (if (= (@state :mode) :drag)
        (let [[offX offY] (@state :offset)
              x (+ (.getX e) offX)
              y (+ (.getY e) offY)]
          (dosync
            (ref-set dragState (assoc @dragState :currentlyDragging true))
            (ref-set dragState (assoc @dragState :p1 [x y]))
            (ref-set dragState (assoc @dragState :p2 [x y]))))))

    (mouseReleased [e]
      (if (= (@state :mode) :drag)
        (let [[baseOffX baseOffY] (@state :offset)
              [sX sY] (@dragState :p1)
              [eX eY] (@dragState :p2)
              newXDif (- eX sX)
              newYDif (- eY sY)]
          (dosync
            (ref-set dragState (assoc @dragState :currentlyDragging false))
            (ref-set dragState (assoc @dragState :p1 [0 0]))
            (ref-set dragState (assoc @dragState :p2 [0 0]))
            (ref-set state (assoc @state :offset [(+ baseOffX newXDif) (+ baseOffY newYDif)]))))))

    (mouseEntered [e])

    (mouseExited [e])

    (mouseClicked [e])))

; Description: Mouse Listener to handle erasing
;
; Parameters: state - world state
;
; Return: MouseListener
(defn eraseListener [state]
  (proxy [MouseListener] []

    (mouseClicked [e]
      (if (= (@state :mode) :erase)
        (let [mX (.getX e)
              mY (.getY e)
              [lineToErase dToLine indexOfSmallest]
                (loop [lines (@state :lines)
                       index 0
                       closestLine nil
                       smallestDistance Integer/MAX_VALUE
                       rIndex -1]
                  (if (empty? lines)
                    [closestLine smallestDistance rIndex]
                    (let [currentLine (first lines)
                          distance (getDistanceToLine currentLine [mX mY])]
                      (if (< distance smallestDistance)
                        (recur (rest lines) (+ index 1) currentLine distance index)
                        (recur (rest lines) (+ index 1) closestLine smallestDistance rIndex)))))
              [obstacleToErase dToobstacle indexOfSmallestOb]
                (loop [obstacles (@state :obstacles)
                       index 0
                       closestobstacle nil
                       smallestDistance Integer/MAX_VALUE
                       rIndex -1]
                  (if (empty? obstacles)
                    [closestobstacle smallestDistance rIndex]
                    (let [currentobstacle (first obstacles)
                          distance (getDistanceToObstacle currentobstacle [mX mY])]
                      (if (< distance smallestDistance)
                        (recur (rest obstacles) (+ index 1) currentobstacle distance index)
                        (recur (rest obstacles) (+ index 1) closestobstacle smallestDistance rIndex)))))]
          (do
            (if (< dToLine ERASEDISTANCE)
              (dosync (ref-set state (assoc @state :lines (dropNth (@state :lines) indexOfSmallest)))))
            (if (< dToobstacle ERASEDISTANCE)
              (dosync (ref-set state (assoc @state :obstacles (dropNth (@state :obstacles) indexOfSmallestOb)))))))))

    (mousePressed [e])

    (mouseReleased [e])

    (mouseEntered [e])

    (mouseExited [e])))

; Description: Mouse Listener to handle placing objects
;
; Parameters: state - world state
;
; Return: MouseListener
(defn placeObstacleListener [state]
  (proxy [MouseListener] []

    (mouseClicked [e]
      (if (= (@state :mode) :place_obstacle)
        (let [baseX (.getX e)
              baseY (.getY e)
              [offX offY] (@state :offset)
              finalX (- baseX offX)
              finalY (- baseY offY)]
        (dosync (ref-set state (assoc @state :obstacles (cons (newObstacle finalX finalY) (@state :obstacles))))))))

    (mousePressed [e])

    (mouseReleased [e])

    (mouseEntered [e])

    (mouseExited [e])))

; Description: KeyListener to handle jumping
;
; Parameters: state - world state
;
; Return: KeyListener
(defn keyBoardListener [state]
  (proxy [KeyListener] []

    (keyPressed [e]
      (if (and (= (.getKeyCode e) 32) (= (state :mode) :play))
        (dosync (ref-set state (assoc @state :rider (jump (state :rider)))))))

    (keyReleased [e])

    (keyTyped [e])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GUI Updaters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description: Paints line
;
; Parameters: g - graphics object
;             line - line object
;             offset - offset based on screen drag; held in game state
;             dragstate - local drag state in world panel
;
; Return: void
;
; sCOff = start of current offset
; eCOff = end of current offset
(defn paintLine [g line offset dragState]
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

; Description: Paints obstacles
;
; Parameters: g - graphics object
;             line - line object
;             offset - offset based on screen drag; held in game state
;             dragstate - local drag state in world panel
;
; Return: void
;
; sCOff = start of current offset
; eCOff = end of current offset
(defn paintObstacle [g obstacle offset dragState]
  (let [[offX offY] offset
        [sCOffX sCOffY] (dragState :p1)
        [eCOffX eCOffY] (dragState :p2)
        cOffX (- eCOffX sCOffX)
        cOffY (- eCOffY sCOffY)
        fullOffX (+ offX cOffX)
        fullOffY (+ offY cOffY)
        [x y] (obstacle :drawCorner)
        toDraw (new java.awt.geom.Rectangle2D$Double (+ fullOffX x) (+ y fullOffY) OBSTACLE_SIDE_LENGTH OBSTACLE_SIDE_LENGTH)]
    (.setColor g Color/red)
    (.fill g toDraw)
    (.draw g toDraw)))

; Description: Paints rider object
;
; Parameters: g - graphics object
;             rider - rider object
;
; Return: void
(defn paintRider [g rider offset dragState]
  (let [[xcord ycord] (rider :cords)
        [offX offY] offset
        [sCOffX sCOffY] (dragState :p1)
        [eCOffX eCOffY] (dragState :p2)
        cOffX (- eCOffX sCOffX)
        cOffY (- eCOffY sCOffY)
        fullOffX (+ offX cOffX)
        fullOffY (+ offY cOffY)
        rad (rider :size)
        toDraw (new java.awt.geom.Ellipse2D$Double (+ fullOffX (- xcord rad)) (+ (- ycord rad) fullOffY) (* 2 rad) (* 2 rad))]
      (.setStroke g (BasicStroke. 1))
      (.setColor g Color/blue)
      (.fill g toDraw)
      (.draw g toDraw)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;GUI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description: Creates the main frame to load our panels into
;
; Parameters: None
;
; Return: JFrame
(defn gameFrame []
  (let [f (proxy [JFrame] []
            (getPreferredSize []
              FRAMEBOUNDS))]
      (.setLayout f (FlowLayout.))
      f))

; Description: Creates a panel for buttons
;
; Parameters: None
;
; Return: JPanel
(defn controlPanel []
  (let [p (proxy [JPanel] []
            (getPreferredSize []
              CONTROLBOUNDS))]
      (.setLayout p (FlowLayout.))
      p))

; Description: Creates the simulation panel - everything happens here
;
; Parameters: state - Specified world state
;
; Return: JPanel
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
                (paintLine g @drawingState (@state :offset) @dragState))

              (if (= (@state :mode) :play)
                (do
                  (.requestFocus this)
                  (applyGamePhysics state)))

              (if (= (@state :mode) :reset)
                (dosync (ref-set state (assoc @state :rider (newRider)))))

              (loop [lines (@state :lines)]
                  (if (empty? lines)
                    :done
                    (do
                      (paintLine g (first lines) (@state :offset) @dragState)
                      (recur (rest lines)))))

              (loop [obstacles (@state :obstacles)]
                  (if (empty? obstacles)
                    :done
                    (do
                      (paintObstacle g (first obstacles) (@state :offset) @dragState)
                      (recur (rest obstacles)))))

              (paintRider g (@state :rider) (@state :offset) @dragState)))]

    (.addMouseListener p (drawListener drawingState state))
    (.addMouseListener p (dragClickListener dragState state))
    (.addMouseListener p (eraseListener state))
    (.addMouseListener p (placeObstacleListener state))
    (.addMouseMotionListener p (dragListener drawingState dragState state))
    (.setLayout p (FlowLayout.))
    (.setBackground p Color/white)
    p))

; Description: Creates a button using the given label and buttonListener
;
; Parameters: label - JLabel to init the button with
;             listener - ActionListener to init the button with
;
; Return: JButton
(defn createButton [label listener]
  (let [b (JButton.)]
    (.addActionListener b listener)
    (.setText b label)
    b))

; Description: Initializes everything, and opens up the frame
;
; Parameters: None
;
; Return: void
(defn createWorld []
  ;create our state
  (def state (ref (newState)))

  ;create our major gui objects
  (def controls (controlPanel))
  (def world (worldPanel state))
  (def frame (gameFrame))

  ;add buttons to the control panel
  (.add controls (createButton "play" (modeSet :play state)))
  (.add controls (createButton "line" (modeSet :line state)))
  (.add controls (createButton "drag" (modeSet :drag state)))
  (.add controls (createButton "erase" (modeSet :erase state)))
  (.add controls (createButton "reset" (modeSet :reset state)))
  (.add controls (createButton "place obstacle" (modeSet :place_obstacle state)))

  ; add keylistener to world
  (.setFocusable world true)
  (.addKeyListener world (keyBoardListener state))

  ; add world panel to our frame
  (.add frame world)

  ; add control panel to our frame
  (.add frame controls)



  ; start world update timer
  (.start (Timer. UPDATERATE (updateTimer world)))

  ; frame logistics
  (.pack frame)
  (.setVisible frame true)
  (.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE))
