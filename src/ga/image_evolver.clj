(ns ga.image-evolver
  (:require (ga [main :as ga])
            (ga [image-util :as img]))  
  (:import
    (java.awt Color Canvas Polygon RenderingHints)    
    (java.io File)    
    (javax.swing JFrame JPanel JFileChooser)))

(set! *warn-on-reflection* true)

(defstruct polygon :color :shape)
(defstruct image :width :height :polygons)


;;;;;;;;;;;;;;;;;;;;;;;helper functions;;;;;;;;;;;;;;;;;;;;;;;

(defn rand-in-range
  "generates a random value within the given range"
  [min max]
    (float (+ (* (Math/random) (- max min)) min)))


(defn gen-polygon [w h]
  (let [color (img/get-random-color)
        p (Polygon.)]
      (dotimes [i (rand-in-range 3 5)]
        (let [x-pos (rand-int w)
              y-pos (rand-int h)]
           (.addPoint p x-pos y-pos)))
      (struct polygon color p)))                  
                      
(defn paint [polygons width height]
  (let [image (img/get-blank-image width height)
        g     (.createGraphics image)]
    
    (doto g       
        (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
        (.setColor Color/black)
        (.fillRect 0 0 width height))    
    (doseq [polygon polygons]       
        (.setColor g (:color polygon))
        (.fillPolygon g (:shape polygon)))
    image))
                                          
;;;;;;;;;;;;;;;;;;;;;;;image specific functions;;;;;;;;;;;;;;;;;;;;;;;
            
(defn image-mater [img1 img2]       
  (let [polygons1 (:polygons img1)
        polygons2 (:polygons img2)
        polygons (into (into [] (take (/ (count polygons1) 2) polygons1))
                         (drop (/ (count polygons2) 2) polygons2))]
      (struct image (:width img1) (:height img1) polygons)))

(defn image-fitness   
  [image target]
    (- 0 (img/calc-distance (paint (:polygons image) (:width image) (:height image)) target)))

(defn image-mutator   
  ([img target threshold fitness]
    (let [w (:width img)
          h (:height img)
          polys (:polygons img)
          size (count polys)]                               
        (loop [new-polys (transient []), pos 0]
          (if (= pos size)
            (struct image w h (persistent! new-polys))
            (recur (assoc! new-polys pos (if (< (rand) threshold) (get polys pos) (gen-polygon w h)))
                   (inc pos))))))
  ([w h member-size]
    (struct image w h
      (vec (for [i (range 0 member-size)] (gen-polygon w h))))))  

(defn -main [args]  

     (def file-chooser (new JFileChooser))  
     (doto file-chooser  
       (.setCurrentDirectory (new File "."))  
       (.showOpenDialog nil))

  (let [frame (JFrame.)   
        canvas (Canvas.)        
        image (img/load-image (.getSelectedFile file-chooser))       
        ;image1 (img/load-image (java.io.File. "evileye.jpg"))          
        image-width (.getWidth image)
        image-height (.getHeight image)  
        pop-size 30
        member-size 100
        width (* 15 image-width)
        height (.getHeight image)
        mutator-struct (struct ga/mutator
                        image-fitness
                        image-mutator
                        image-mater
                        image
                        0.05)]                  
                
       (doto frame
         (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
         (.setTitle (str "Evolving " (.getName (.getSelectedFile file-chooser))))
         (.setBounds 0,0,width height)
         (.setResizable false)
         (.add canvas)
         (.setVisible true))
        
       (doto canvas
         (.createBufferStrategy 2)
         (.. (getBufferStrategy) (getDrawGraphics) (setColor Color/black))
         (.. (getBufferStrategy) (getDrawGraphics) (fillRect 0 0 (.getWidth image) (.getHeight image)))
         (.setVisible true)
         (.requestFocus))       
      
       (loop [population (ga/init-population mutator-struct image-width height pop-size member-size)] 
          (let [ranked (vec (ga/rank population mutator-struct))]  
            (dotimes [i (if (> pop-size 15) 15 pop-size)]           
              (img/draw canvas 
                        (paint (:polygons (:value @(get ranked i))) image-width image-height) 
                        (* i image-width) 
                        0)
              (img/draw-string canvas
                        (str (int (:fitness @(get ranked i))))
                        (+ (* i image-width) 10)
                        10))              
            (recur (ga/evolve ranked mutator-struct))))             
     )) 
     

(-main nil)                         