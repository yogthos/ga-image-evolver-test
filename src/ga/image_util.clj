(ns ga.image-util
  (:import
    (java.awt Canvas Graphics Dimension Color Polygon GraphicsEnvironment Transparency Toolkit)
    (java.awt.image BufferStrategy BufferedImage PixelGrabber WritableRaster)
    (javax.media.jai.iterator RandomIterFactory)
    (java.awt AlphaComposite Polygon RenderingHints)
    (javax.imageio ImageIO)
    (java.awt.image.renderable ParameterBlock))
  (:gen-class))
  
(defn zip
  "helper function which pairs values in two lists xs ys"
  [xs ys]
  (loop [pairs (transient []) l1 xs l2 ys]
    (if (or (empty? l1) (empty? l2))
      (persistent! pairs)
      (recur (conj! pairs [(first l1) (first l2)]) (rest l1) (rest l2)))))

(defn- average-around [image px py]
  (let [iterator (RandomIterFactory/create image nil)
        width (.getWidth image)
        height (.getHeight image)
        base-size (if (< width height) width height)
        sample-size 15        
        x-min-tmp (- (* px base-size) sample-size)
        x-max-tmp (+ (* px base-size) sample-size)
        y-min-tmp (- (* py base-size) sample-size) 
        y-max-tmp (+ (* py base-size) sample-size)        
        x-min (if (neg? x-min-tmp) 0 x-min-tmp)
        x-max (if (< x-max-tmp width) x-max-tmp width)
        y-min (if (neg? y-min-tmp) 0 y-min-tmp)
        y-max (if (< y-max-tmp height) y-max-tmp height)        
        x-range (range x-min x-max)       
        y-range (range y-min y-max)       
        accum (to-array [0 0 0])
        pixel (make-array (. Double TYPE) x-max)
        num-pixels (* x-max y-max)]
        
        (doseq [x x-range]
          (doseq [y y-range] 
            (.getPixel iterator (int x) (int y) pixel)
            (aset accum 0 (+ (aget accum 0) (aget pixel 0)))
            (aset accum 1 (+ (aget accum 0) (aget pixel 1)))
            (aset accum 2 (+ (aget accum 0) (aget pixel 2)))))
            
        (aset accum 0 (/ (aget accum 0) num-pixels))
        (aset accum 1 (/ (aget accum 1) num-pixels))
        (aset accum 2 (/ (aget accum 2) num-pixels))
        
        (Color. (int (aget accum 0)) (int (aget accum 1)) (int (aget accum 2)))))
        
(defn- get-signature [image]
  (let [prop (vec (map float [(/ 1 10) (/ 3 10) (/ 5 10) (/ 7 10) (/ 9 10)])) 
        sig (for [x (range 0 5)]
              (for [y (range 0 5)] (average-around image (get prop x) (get prop y))))]        
   sig))

(defn- compare-pixels [p1 p2]
  (let [r1 (.getRed p1)
        g1 (.getGreen p1)
        b1 (.getBlue p1)
        r2 (.getRed p2)
        g2 (.getGreen p2)
        b2 (.getBlue p2)]
        (Math/sqrt (+ (* (- r1 r2) (- r1 r2)) 
                      (* (- g1 g2) (- g1 g2))
                      (* (- b1 b2) (- b1 b2))))))

(defn- compare-colors [c1 c2]
  (pmap #(compare-pixels (first %1) (second %1)) (zip c1 c2)))

(defn calc-distance [image target]
  (let [sig-target (get-signature target)
        sig-image (get-signature image)]        
        (reduce + (reduce concat (pmap #(compare-colors (first %1) (second %1)) (zip sig-image sig-target))))))       
        
(defn blend [bi1 bi2 weight]
  (let [width (.getWidth bi1)
        height (.getHeight bi1)
        bi3 (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
        rgbim1 (make-array (. Integer TYPE) width)
        rgbim2 (make-array (. Integer TYPE) width)
        rgbim3 (make-array (. Integer TYPE) width)]
        
        (doseq [row (range 0 height)]
          (.getRGB bi1 0 row width 1 rgbim1 0 width)
          (.getRGB bi2 0 row width 1 rgbim2 0 width)

          (doseq [col (range 0 width)]
            (let [rgb1 (aget rgbim1 col)
                  r1 (bit-and (bit-shift-right rgb1 16) 255)          
                  g1 (bit-and (bit-shift-right rgb1 8) 255)
                  b1 (bit-and rgb1 255)
                                    
                  rgb2 (aget rgbim2 col)
                  r2 (bit-and (bit-shift-right rgb2 16) 255)          
                  g2 (bit-and (bit-shift-right rgb2 8) 255)
                  b2 (bit-and rgb2 255)
                  
                  r3 (int (+ (* r1 weight) (* (- 1.0 weight) r2)))
                  g3 (int (+ (* g1 weight) (* (- 1.0 weight) g2)))
                  b3 (int (+ (* b1 weight) (* (- 1.0 weight) b2)))]
               (aset rgbim3 col (bit-or (bit-or (bit-shift-left r3 16) (bit-shift-left g3 8)) b3))))
            (.setRGB bi3 0 row width 1 rgbim3 0 width))         
          bi3))     

(defn get-compatible [width height]
  (let [ge (GraphicsEnvironment/getLocalGraphicsEnvironment)]
       (.. ge (getDefaultScreenDevice)
              (getDefaultConfiguration)
              (createCompatibleImage 
                width
                height
                (Transparency/BITMASK)))))

(defn load-image [file]
  (let [image (ImageIO/read file)
        ge (GraphicsEnvironment/getLocalGraphicsEnvironment)
        compatible (get-compatible (.getWidth image) (.getHeight image))
        g (.getGraphics compatible)]
        (.drawImage g image 0 0 nil)
        compatible))

(defn draw-string [#^Canvas canvas #^String text x-offset y-offset]
    (let [buffer (.getBufferStrategy canvas)        
          g     (.getDrawGraphics buffer)]     
      (try 
        (doto g
          (.setColor Color/green)
          (.drawString text x-offset y-offset))           
        (finally (.dispose g)))
      (if (not (.contentsLost buffer))
        (. buffer show))
      (.. Toolkit (getDefaultToolkit) (sync))))

(defn draw [#^Canvas canvas #^BufferedImage image x-offset y-offset]
    (let [buffer (.getBufferStrategy canvas)        
          g     (.getDrawGraphics buffer)]     
      (try 
        (doto g
          (.drawImage image x-offset y-offset nil))           
        (finally (.dispose g)))
      (if (not (.contentsLost buffer))
        (. buffer show))
      (.. Toolkit (getDefaultToolkit) (sync))))

(defn get-blank-image [width height]
  (BufferedImage. width height BufferedImage/TYPE_INT_ARGB))
  
(defn get-image-with-opacity [image opacity]
  (let [new-image (get-blank-image (.getWidth image) (.getHeight image))
            g     (.createGraphics new-image)
            ac    (AlphaComposite/getInstance AlphaComposite/SRC_OVER opacity)]
          (try  
            (doto g
              (.setComposite ac)
              (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
              (.drawImage image nil 0 0))
              (finally (.dispose g)))
          new-image))

(defn get-random-color []
  (Color. (float (rand)) (float (rand)) (float (rand)) (float (rand))))                    
          