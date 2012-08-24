(ns ga.image-evolver
  (:require [ga.image-util :as img])  
  (:import [java.awt Color Canvas Graphics2D Polygon Rectangle RenderingHints]
           java.awt.image.BufferedImage
           java.awt.geom.Ellipse2D$Double
           [javax.swing JFrame JPanel JFileChooser]    
           java.io.File))

;(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;helper functions;;;;;;;;;;;;;;;;;;;;;;;

(defn rand-in-range
  "generates a random value within the given range"
  [min max]
    (float (+ (* (Math/random) (- max min)) min)))

(defn rand-offset [i max]
  ((if (< (rand) 0.5) + -) i (rand-int (/ max 5)))
  #_(let [v ((if (< (rand) 0.5) + -) i (rand-int (/ max 3)))]
    (if (< 0 v) 0 (if (> v (dec max)) (dec max) v))) )


(defn gen-shape [w h size]  
  {:color (img/get-random-color)
   :shape (new Ellipse2D$Double (rand w) (rand h) (rand 15) (rand 15))}
  #_(let [color (img/get-random-color)
        p (Polygon.)
        x-start (rand-int w)
        y-start (rand-int h)]
    (dotimes [i (rand-in-range 3 4)]
      (let [x-pos (int (rand-offset x-start w))
            y-pos (int (rand-offset y-start h))]
        (.addPoint p x-pos y-pos)))
    {:color color :shape p}))                  
                      
(defn paint [polygons width height]
  (let [^BufferedImage image (img/get-blank-image width height)
        ^Graphics2D    g     (.createGraphics image)]
    
    (doto g       
        (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
        (.setColor Color/black)
        (.fillRect 0 0 width height))    
    (doseq [polygon polygons]       
        (.setColor g (:color polygon))
        (.fill g (:shape polygon)))
    image))
                                          
;;;;;;;;;;;;;;;;;;;;;;;ga functions;;;;;;;;;;;;;;;;;;;;;;;
(defn rand-in-range
  "generates a random value within the given range"
  [min max]
  (int (+ (* (Math/random) (inc (- max min))) min)))

(defn mutate 
  "randomly mutates values in members of the population using the mutator function"
  [population mutator threshold fitness target]
  (for [member population]
    (if (< (rand) threshold)
      (let [value (map (partial mutator (:fitness member)) (:value member))]
        {:value value
         :fitness (fitness value target)})
      member)))

(defn rank
  "ranks the population by fitness"
  [population]
  (reverse (sort-by :fitness population)))

(defn update-vals
  "randomly selects a value from either the first or the second memeber for each position,
   preferring the first member, as the front of the population is more fit"
  [fitness target {v1 :value} {v2 :value}]  
  (let [value (map #(if (> (rand) 0.4) %1 %2) v1 v2)]
    {:value value :fitness (fitness value target)}))

(defn mate
  "splits the population in half and mates all the members"
  [population fitness target]
  (apply map 
         (partial update-vals fitness target) 
         (split-at (/ (count population) 2) population)))

(defn evolve-step
  "mutate the population, then promote top members and add mated members to the end"
  [size population mutator threshold fitness target]
  (let [mutated (rank (mutate population mutator threshold fitness target))        
        promote-size (/ size 5)
        keep-size (- (/ size 2) promote-size)
        [xs ys] (split-at keep-size mutated)]
    (concat xs (take promote-size ys) (mate mutated fitness target))))

(defn gen-member
  "generates a new member for the population"
  [mutator fitness member-size target]
  (let [value (take member-size (repeatedly #(mutator)))] 
    {:value value :fitness (fitness value target)}))

(defn init-population
  "creates a new population"
  [size member-size mutator fitness target]
  (rank (take size (repeatedly #(gen-member mutator fitness member-size target)))))

(defn evolve
  "evolves the population until at least one member is fit"
  [size member-size threshold mutator fitness target]
  (loop [population (init-population size member-size mutator fitness target)]
    (println (first population))
    (if (zero? (:fitness (first population))) 
      population 
      (recur (evolve-step size population mutator threshold fitness target)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn -main [& args]  

     (def file-chooser (new JFileChooser))  
     (doto file-chooser  
       (.setCurrentDirectory (new File "."))  
       (.showOpenDialog nil))

  (let [frame (JFrame.)   
        canvas (Canvas.)        
        image (img/load-image (.getSelectedFile file-chooser))       
        image-width (.getWidth image)
        image-height (.getHeight image)                  
        width (* 5 image-width)]                  
                
       (doto frame
         (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
         (.setTitle (str "Evolving " (.getName (.getSelectedFile file-chooser))))
         (.setBounds 0 0 width image-height)
         (.setResizable false)
         (.add canvas)
         (.setVisible true))
        
       (doto canvas
         (.createBufferStrategy 2)
         (.. (getBufferStrategy) (getDrawGraphics) (setColor Color/black))
         (.. (getBufferStrategy) (getDrawGraphics) (fillRect 0 0 image-width image-height))
         (.setVisible true)
         (.requestFocus))       
      
       (let [size 500
             polygons 500
             threshold 0.05
             mutator (fn 
                       ([] (gen-shape image-width image-height image-width))
                       ([fitness shape] (gen-shape image-width image-height (/ fitness 10000000))))
             fitness #(- 0 (img/cmp-img (paint %1 image-width image-height) %2))] 
         (loop [population (init-population size polygons mutator fitness image)]
           (dotimes [i (if (> size 5) 5 size)]           
               (img/draw canvas 
                         (paint (:value (first (drop i population))) image-width image-height) 
                         (* i image-width) 
                         0
                         img/draw-image)
               (img/draw canvas
                         (str (int (:fitness (first (drop i population)))))
                         (+ (* i image-width) 10)
                         10
                         img/draw-string))
           (if (zero? (:fitness (first population))) 
             population 
             (recur (evolve-step size population mutator threshold fitness image))))))) 

(-main)
