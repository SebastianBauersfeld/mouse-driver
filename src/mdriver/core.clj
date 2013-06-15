(ns mdriver.core)

(defn make-robot []
  (java.awt.Robot.))

(def default-speed 1000)
(def default-granularity 1000)
(def default-smoothness 20)
(def globalRobot (make-robot))

(defn pause [duration]
  (java.lang.Thread/sleep (* duration 1000)))

(defn get-cursor []
  (let [cursor (-> (java.awt.MouseInfo/getPointerInfo) .getLocation)]
    [(.getX cursor) (.getY cursor)]))

(defn set-cursor [x y]
  (.mouseMove globalRobot x y))

(defn seconds []
  (/ (java.lang.System/currentTimeMillis) 1000))

; speed in units per second
(defn move-cursor 
  ([x y speed granularity]
   (let [[cx cy] (get-cursor)
         width (- x cx)
         height (- y cy)
         duration (/ (java.lang.Math/sqrt (+ (* width width) (* height height))) speed)
         start (seconds)]
     (loop []
       (pause (/ 1 granularity))
       (let [t (/ (- (seconds) start) duration)]
         (set-cursor (+ cx (* (min t 1) width)) (+ cy (* (min t 1) height)))
         (when (< t 1)
           (recur))))))
  ([x y speed]
   (move-cursor x y speed default-granularity))
  ([x y]
   (move-cursor x y default-speed default-granularity)))

(defn left-mouse-down []
  (.mousePress globalRobot java.awt.event.InputEvent/BUTTON1_MASK))

(defn left-mouse-up []
  (.mouseRelease globalRobot java.awt.event.InputEvent/BUTTON1_MASK))

(defn right-mouse-down []
  (.mousePress globalRobot java.awt.event.InputEvent/BUTTON3_MASK))

(defn right-mouse-up []
  (.mouseRelease globalRobot java.awt.event.InputEvent/BUTTON3_MASK))

(defn gammas
  ([n]
   (gammas [(/ 1 2)] n))
  ([vect n]
   (let [i (count vect)
         new-vect (conj vect (/ 1 (- (if (= i n) 2 4) (last vect))))]
     (if (= i n)
       new-vect
       (recur new-vect n)))))

(defn deltas
  ([ys gammas]
   (deltas [(* 3 (- (second ys) (first ys)) (first gammas))] ys gammas))
  ([vect ys gammas]
   (let [n (- (count ys) 1)
         i (count vect)
         i+1 (+ i 1)
         i-1 (- i 1)
         new-vect (conj vect (* (- (* 3 (- (nth ys (if (= i n) i i+1)) (nth ys i-1))) (last vect)) (nth gammas i)))]
     (if (= i n)
       new-vect
       (recur new-vect ys gammas)))))

(defn Ds
  ([deltas gammas]
   (Ds [(last deltas)] deltas gammas))
  ([vect deltas gammas]
   (let [n (- (count gammas) 1)
         i (- n (count vect))
         new-vect (conj vect (- (nth deltas i) (* (nth gammas i) (last vect))))]
     (if (= i 0)
       (vec (reverse new-vect))
       (recur new-vect deltas gammas)))))

(defn polynoms [ys]
  (let [n (- (count ys) 1)
        gammas (gammas n)
        deltas (deltas ys gammas)
        Ds (Ds deltas gammas)]
    (loop [vect []
           i 0]
      (let [i+1 (+ i 1)
            new-vect (conj
                       vect
                       ;calculate final coefficients of the (cubic) polynom
                       [(+ (* 2 (- (nth ys i) (nth ys i+1))) (nth Ds i) (nth Ds i+1))                 ;a
                        (- (* 3 (- (nth ys i+1) (nth ys i))) (* 2 (nth Ds i)) (nth Ds i+1))           ;b
                        (nth Ds i)                                                                    ;c
                        (nth ys i)])]                                                                 ;d
        (if (< i+1 n)
          (recur new-vect (+ i 1))
          new-vect)))))


;evaluate the polynom (ax^3 + bx^2 + cx + d) at x
(defn eval-poly [[a b c d] x]
  (+ d (* x (+ c (* x (+ b (* x a)))))))

; evaluate a list of cubic polynoms at all x coordinates within xs
(defn poly-seq [polys xs]
  (if (empty? polys)
    ()
    (concat (for [x xs] (eval-poly (first polys) x)) (poly-seq (rest polys) xs))))

;takes a vector of points and returns a vector of cubic polynoms (natural cubic splines)
(defn calc-spline 
  ([points interpolations]
   (let [xs (vec (for [[x _] points] x))
         ys (vec (for [[_ y] points] y))
         gran (/ 1 interpolations)
         res-xs (poly-seq (polynoms xs) (range 0 1 gran))
         res-ys (poly-seq (polynoms ys) (range 0 1 gran))
         res-points (map vector res-xs res-ys)]
     (concat res-points (list [(last xs) (last ys)]))))
  ([points]
   (calc-spline points default-smoothness)))

; moves the cursor through a list of points with a given speed
(defn move-through
  ([points speed]
   (loop [pointseq points]
     (when (not (empty? pointseq))
       (let [[x y] (first pointseq)]
         (move-cursor x y speed)
         (recur (rest pointseq))))))
  ([points]
   (move-through points default-speed)))

; draws an ellipse
(defn ellipse [x y width height]
  (let [cx (+ x (/ width 2))
        cy (+ y (/ height 2))]
    (calc-spline [[cx y] [x cy] [cx (+ y height)] [(+ x width) cy] [cx y] [x cy]])))

; connects a list of points with lines
(defn draw
  ([points speed down up]
   (let [[x y] (first points)]
     (move-cursor x y speed)
     (down)
     (move-through points speed)
     (up)))
  ([points]
   (draw points default-speed left-mouse-down left-mouse-up)))

; draws various symbols (letters, rectangles, filled rectangles, smileys)
(defn widget
  ([ch x y width height speed down up thickness]
   (letfn [(scale [points]
             (for [[relx rely] points] [(+ x (* relx width)) (+ y (* rely height))]))
           (dr [points]
             (draw (scale points) speed down up))
           (dri [points]
             (draw (calc-spline (scale points)) speed down up))
           (A []
             (dri [[0 1] [0.5 0] [1 1]])
             (dri [[0 0.5] [1 0.5]]))
           (B []
             (dri [[0 0] [0 1]])
             (dri [[0 0] [1 0.2] [1 0.4] [0 0.5] [1 0.7] [1 0.9] [0 1] ]))
           (C []
             (dri [[1 0.15] [0.5 0] [0 0.5] [0.5 1] [1 0.85]  ]))
           (D []
             (dri [[0 0] [0 1]])
             (dri [[0 0] [1 0.25] [1 0.75] [0 1] ]))
           (E []
             (dri [[0 0] [0 1]])
             (dri [[0 0] [1 0]])
             (dri [[0 0.5] [0.7 0.5]])
             (dri [[0 1] [1 1]]))
           (F []
             (dri [[0 0] [0 1]])
             (dri [[0 0] [1 0]])
             (dri [[0 0.5] [0.7 0.5]]))
           (G []
             (dri [[1 0.15] [0.5 0] [0 0.5] [0.5 1] [1 0.85] [1 0.6] [0.7 0.6] ]))
           (H []
             (dri [[0 0] [0 1]])
             (dri [[1 0] [1 1]])
             (dri [[0 0.5] [1 0.5]]))
           (I []
             (dri [[0.5 0] [0.5 1]]))
           (J []
             (dri [[0 0] [0.5 0] [0.5 0.8] [0 1] [0 0.8]]))
           (K []
             (dri [[0 0] [0 1]])
             (dri [[0 0.5] [1 0]])
             (dri [[0 0.5] [1 1]]))
           (L []
             (dri [[0 0] [0 1] [1 1]]))
           (M []
             (dri [[0 1] [0 0] [0.5 0.4] [1 0] [1 1]]))
           (N []
             (dri [[0 1] [0 0]])
             (dri [[0 0] [1 1]])
             (dri [[1 1] [1 0]]))
           (O []
             (dri [[0.5 0] [0 0.5] [0.5 1] [1 0.5] [0.5 0]]))
           (P []
             (dri [[0 0] [0 1]])
             (dri [[0 0] [1 0.2] [1 0.4] [0 0.5]]))
           (Q []
             (dri [[0.5 0] [0 0.5] [0.5 1] [1 0.5] [0.5 0]])
             (dri [[0.7 0.7] [1 1]]))
           (R []
             (dri [[0 0] [0 1]])
             (dri [[0 0] [1 0.2] [1 0.4] [0 0.5] [1 1]]))
           (S []
             (dri [[1 0.2] [0.5 0] [0 0.3] [0.5 0.4] [1 0.6] [1 0.8] [0.5 1] [0 1]]))
           (T []
             (dri [[0 0] [1 0]])
             (dri [[0.5 0] [0.5 1]]))
           (U []
             (dri [[0 0] [0 0.5] [0.5 1] [1 0.5] [1 0]]))
           (V []
             (dri [[0 0] [0.5 1]])
             (dri [[0.5 1] [1 0]]))
           (W []
             (dri [[0 0] [0.25 1] [0.5 0.7] [0.75 1] [1 0]]))
           (X []
             (dri [[0 0] [1 1]])
             (dri [[0 1] [1 0]]))
           (Y []
             (dri [[0 0] [0.5 0.5] [1 0]])
             (dri [[0.5 0.5] [0.5 1]]))
           (Z []
             (dri [[0 0] [1 0]])
             (dri [[1 0] [0 1]])
             (dri [[0 1] [1 1]]))
           (Bang []
             (dri [[0.5 0] [0.5 0.8]])
             (dri [[0.5 1] [0.5 1]]))
           (Space []) ; does not do anything (just skip a letter)
           (Smiley []
             (dr (ellipse 0 0 1 1))           ; face
             (dr (ellipse 0.3 0.2 0.1 0.1))   ; left eye
             (dr (ellipse 0.6 0.2 0.1 0.1))   ; right eye
             (dri [[0.2 0.6] [0.5 0.8] [0.8 0.6]])) ; mouth
           (Rect []
             (dr [[0 0] [1 0] [1 1] [0 1] [0 0]]))
           (FilledRect []
             (Rect)
             (loop [ticks (range 0 1 thickness)]
               (when-not (empty? ticks)
                 (dr [[(first ticks) 0] [0 (first ticks)]])
                 (recur (rest ticks))))
             (loop [ticks (range 0 1 thickness)]
               (when-not (empty? ticks)
                 (dr [[1 (first ticks)] [(first ticks) 1]])
                 (recur (rest ticks)))))
           
            ]
      (let [ctof {\A A \B B \C C \D D \E E \F F \G G \H H \I I \J J \K K \L L
                  \M M \N N \O O \P P \Q Q \R R \S S \T T \U U \V V \W W \X X
                  \Y Y \Z Z \space Space \! Bang "Smiley" Smiley "Rect" Rect
                  "FilledRect" FilledRect}]
        ((ctof ch)))))
([ch x y width height speed]
 (widget ch x y width height speed left-mouse-down left-mouse-up 0.07)))


; paints the hacker school logo
(defn hackerschool [x y width height speed]
  (letfn [(grect [id rx ry rwidth rheight down up thickness]
            (widget id
                    (+ x (* rx width)) (+ y (* ry height))
                    (* rwidth width) (* rheight height)
                    speed down up thickness))
          (rect [rx ry rwidth rheight]
            (grect "Rect" rx ry rwidth rheight left-mouse-down left-mouse-up 0.07))
          (black-rect [rx ry rwidth rheight thickness]
            (grect "FilledRect" rx ry rwidth rheight left-mouse-down left-mouse-up thickness))
          (white-rect [rx ry rwidth rheight thickness]
            (grect "FilledRect" rx ry rwidth rheight right-mouse-down right-mouse-up thickness))]
    (rect 0 0 1 0.65) ; screen frame
    (black-rect 0.1 0.1 0.8 0.45 0.06)
    (white-rect 0.1 0.15 0.05 0.05 0.3)
    (pause 0.2)
    (white-rect 0.3 0.15 0.05 0.05 0.3)
    (pause 0.2)
    (white-rect 0.5 0.15 0.05 0.05 0.3)
    (pause 0.2)
    (white-rect 0.2 0.3 0.15 0.05 0.3)
    (pause 0.2)
    (white-rect 0.5 0.3 0.15 0.05 0.3)
    (pause 0.2)
    (black-rect 0.35 0.65 0.3 0.1 0.1)
    (black-rect 0.1 0.75 0.8 0.06 0.2)
    (black-rect 0 0.81 1 0.19 0.1)
    (let [wrw (/ 1 20)]
      (white-rect (* 3 wrw) 0.81 wrw wrw 0.4)
      (pause 0.2)
      (white-rect (* 7 wrw) 0.81 wrw wrw 0.4)
      (pause 0.2)
      (white-rect (* 11 wrw) 0.81 wrw wrw 0.4)
      (pause 0.2)
      (white-rect (* 15 wrw) 0.81 wrw wrw 0.4)
      (pause 0.2)
      (white-rect (* 1.5 wrw) (+ 0.83 wrw) wrw wrw 0.4)
      (pause 0.2)
      (white-rect (* 5.5 wrw) (+ 0.83 wrw) wrw wrw 0.4)
      (pause 0.2)
      (white-rect (* 9.5 wrw) (+ 0.83 wrw) wrw wrw 0.4)
      (pause 0.2)
      (white-rect (* 13.5 wrw) (+ 0.83 wrw) wrw wrw 0.4))))


; draws an arbitrary string (only capital letters)
(defn text [string x y width height xgap speed]
  (when (not (empty? string))
    (widget (first string) x y width height speed)
    (recur (rest string) (+ x width xgap) y width height xgap speed)))


(defn demo []
  (pause 2)
  (hackerschool 200 200 200 200 1000)
  (pause 0.3)
  (text "HACKER SCHOOL" 200 450 40 40 10 800)
  (text "NEVER GRADUATE!!!" 250 550 20 20 5 600)
  (widget "Smiley" 250 600 100 100 1000))


(defn -main
  [& args]
  (println "those are the provided arguments" (apply str args)))
