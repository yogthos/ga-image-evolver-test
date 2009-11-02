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

(defn- grab-pixels
  "Returns an array containing the pixel values of image."
  [image]
  (let [w (.getWidth image)
        h (.getHeight image)
        pixels (make-array (Integer/TYPE) (* w h))]
    (.grabPixels (PixelGrabber. image 0 0 w h pixels 0 w))
    pixels))

(defn- compare-colors [pixel1 pixel2]
  (let [color1 (Color. pixel1)
        color2 (Color. pixel2)
        dr (- (.getRed color1) (.getRed color2))
        dg (- (.getGreen color1) (.getGreen color2))
        db (- (.getBlue color1) (.getBlue color2))]
     (* dr dr) (* dg dg) (* db db)))

(defn cmp-img [image1 image2]
    (let [pixels1 (grab-pixels image1)
          pixels2 (grab-pixels image2)]
      (loop [i (int 0)
             lms (int 0)]
        (if (> (inc i) (alength pixels1))
          lms
          (recur (unchecked-inc i) 
                 (int (+ lms (compare-colors (aget pixels1 i) (aget pixels2 i)))))))))


(comment
(defn compare [image1 image2]
    (let [pixels1 (grab-pixels image1)
          pixels2 (grab-pixels image2)]
      (loop [i (int 0)
             lms (int 0)]
        (if (> i (alength pixels1))
          lms
          (let [color1 (Color. (aget pixels1 i))
                color2 (Color. (aget pixels2 i))
                dr (- (.getRed color1) (.getRed color2))
                dg (- (.getGreen color1) (.getGreen color2))
                db (- (.getBlue color1) (.getBlue color2))]
            (recur (unchecked-inc i) (int (+ lms (* dr dr) (* dg dg) (* db db)))))))))
)

        
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

(defn draw-image [#^Graphics g #^BufferedImage image x-offset y-offset]	
	(.drawImage g image x-offset y-offset nil))           

(defn draw-string [#^Graphics g #^String text x-offset y-offset]
   (doto g
      (.setColor Color/green)
      (.drawString text x-offset y-offset)))           

(defn draw [#^Canvas canvas image x-offset y-offset draw-fn]
    (let [buffer (.getBufferStrategy canvas)        
          g     (.getDrawGraphics buffer)]     
      (try (draw-fn g image x-offset y-offset)
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
          