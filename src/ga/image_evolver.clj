(ns ga.image-evolver
  (:require (ga [main :as ga])
  					(ga [image-util :as img]))  
	(:import
  	(java.awt Color Canvas Polygon) 	  
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
			(dotimes [i (rand-in-range 2 5)]
				(let [x-pos (rand-int w)
						  y-pos (rand-int h)]
					 (.addPoint p x-pos y-pos)))
  	  (struct polygon color p)))	   							
		  								
(defn paint [polygons width height]
	(let [image (img/get-blank-image width height)
	      g     (.createGraphics image)]
	  
	  (doto g
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
			  polygons (concat (take (/ (count polygons1) 2) polygons1)
				 			   			   (drop (/ (count polygons2) 2) polygons2))]
			(struct image (:width img1) (:height img1) (into-array polygons))))

(defn image-fitness 	
	[image target]
		(- 0 (img/calc-distance (paint (:polygons image) (:width image) (:height image)) target)))

(defn image-mutator 	
	([image target threshold fitness]
		(let [w (:width image)
				  h (:height image)]						  									
				(dotimes [i (count (:polygons image))]
					(when (< (rand) threshold)									
     			  (aset (:polygons image) i (gen-polygon w h))))
     		image))
  ([width height member-size]
    (struct image width height (into-array (map (fn[_] (gen-polygon width height)) 
							      														(range 0 member-size)))))) 	

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
        pop-size 50
        member-size 500
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
			
			
			 ;(let [i1 (image-mutator image-width image-height member-size)
			 ;			 i2 (image-mutator image-width image-height member-size)]
			 ;		(img/draw canvas (paint (:polygons (image-mater i1 i2)) image-width image-height) 0 0))
			
       (loop [population (ga/init-population mutator-struct width height pop-size member-size)] 
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