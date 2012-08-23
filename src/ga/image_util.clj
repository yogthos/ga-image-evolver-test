(ns ga.image-util
  (:import
    (java.awt Canvas Graphics Graphics2D Dimension Color Polygon GraphicsEnvironment Transparency Toolkit)
    (java.awt.image BufferStrategy BufferedImage PixelGrabber WritableRaster)
    (javax.media.jai.iterator RandomIterFactory)
    (java.awt AlphaComposite Polygon RenderingHints)
    java.awt.image.renderable.ParameterBlock
    javax.imageio.ImageIO
    java.io.File))
  
(set! *warn-on-reflection* true)

(defn- grab-pixels
  "Returns an array containing the pixel values of image."
  [^BufferedImage image]
  (let [w (.getWidth image)
        h (.getHeight image)
        pixels (int-array (* w h))]
    (.grabPixels (PixelGrabber. image 0 0 w h pixels 0 w))
    pixels))

(defn- compare-colors [pixel1 pixel2]
  (let [color1 (Color. pixel1)
        color2 (Color. pixel2)
        dr (- (.getRed color1) (.getRed color2))
        dg (- (.getGreen color1) (.getGreen color2))
        db (- (.getBlue color1) (.getBlue color2))]
     (* dr dr) (* dg dg) (* db db)))

(defn cmp-img [^BufferedImage image1 ^BufferedImage image2]
    (let [^ints pixels1 (grab-pixels image1)
          ^ints pixels2 (grab-pixels image2)]
      (loop [i (int 0)
             lms (int 0)]
        (if (> (inc i) (alength pixels1))
          lms
          (recur (unchecked-inc i) 
                 (int (+ lms (compare-colors (aget pixels1 i) (aget pixels2 i)))))))))


(comment
(defn compare [^BufferedImage image1 ^BufferedImage image2]
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

        
(defn blend [^BufferedImage bi1 ^BufferedImage bi2 weight]
  (let [width (.getWidth bi1)
        height (.getHeight bi1)
        bi3 (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
        ^ints rgbim1 (int-array  width)
        ^ints rgbim2 (int-array  width)
        ^ints rgbim3 (int-array  width)]
        
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

(defn load-image [^File file]
  (let [^BufferedImage image (ImageIO/read file)
        ge (GraphicsEnvironment/getLocalGraphicsEnvironment)
        ^BufferedImage compatible (get-compatible (.getWidth image) (.getHeight image))
        g (.getGraphics compatible)]
        (.drawImage g image 0 0 nil)
        compatible))

(defn draw-image [^Graphics g ^BufferedImage image x-offset y-offset]	
	(.drawImage g image x-offset y-offset nil))           

(defn draw-string [^Graphics g ^String text x-offset y-offset]
   (doto g
      (.setColor Color/green)
      (.drawString text (int x-offset) (int y-offset))))           

(defn draw [^Canvas canvas image x-offset y-offset draw-fn]
    (let [buffer (.getBufferStrategy canvas)        
          g     (.getDrawGraphics buffer)]     
      (try (draw-fn g image x-offset y-offset)
        (finally (.dispose g)))
      (if (not (.contentsLost buffer))
        (. buffer show))
      (.. Toolkit (getDefaultToolkit) (sync))))

(defn get-blank-image [width height]
  (BufferedImage. width height BufferedImage/TYPE_INT_ARGB))
  
(defn get-image-with-opacity [^BufferedImage image opacity]
  (let [^BufferedImage new-image (get-blank-image (.getWidth image) (.getHeight image))
            ^Graphics2D g          (.createGraphics new-image)
            ^AlphaComposite ac   (AlphaComposite/getInstance AlphaComposite/SRC_OVER opacity)]
          (try  
            (doto g
              (.setComposite ac)
              (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
              (.drawImage image nil 0 0))
              (finally (.dispose g)))
          new-image))

(defn get-random-color []
  (Color. (float (rand)) (float (rand)) (float (rand)) (float (rand))))                    
