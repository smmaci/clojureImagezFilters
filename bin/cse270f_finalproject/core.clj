(ns cse270f-finalproject.core)
  (use 'mikera.image.core)
  (use 'mikera.image.colours)
  
(comment
  This program was written by Samuel Mallamaci
  I implemented the library 'imagez' by mikera, which can be found at https://github.com/mikera/imagez
  
  My goal was to write my own image filters, but when I found that 'imagez' contained
  many filtering functions, I decided to ignore them and bravely forge my own path.
  
  While my filters are not incredibly 'efficient', I still created them with love
  
  I have a filter for reducing the red-level, the blue-level, and the green-level for an image,
  and a filter in progress for removing redeye.
  
  I also made a filter to create neat andy-warhol-ish pop-art out of png's with transparency)

(def baseImg(load-image-resource "luigi.png"));;This loads the baseimage for alterations
(def baseWd (width baseImg))
(def baseHg (height baseImg))
(def newImg (new-image baseWd baseHg true))


(defn reduce-red [reductionAmount img]
  "Reduces the red value of an image by the given amount, ranging from 0 to 255"
  (loop [y 0] 
    (when (< y baseHg)
      (loop [x 0]
        (when (< x baseWd)
          (let [currentPixel (get-pixel img x y)] 
            (set-pixel img x y 
              (rgb 
                (if (< (- (extract-red currentPixel) reductionAmount) 0)
                 0
                 (/ (- (extract-red currentPixel) reductionAmount) 255))
                (/ (extract-green currentPixel) 255)
                (/ (extract-blue currentPixel) 255))))
          (recur (inc x))))
      (recur (inc y)))))

(defn reduce-green [reductionAmount img]
  "Reduces the green value of an image by the given amount, ranging from 0 to 255"
  (loop [y 0] 
    (when (< y baseHg)
      (loop [x 0]
        (when (< x baseWd)
          (let [currentPixel (get-pixel img x y)] 
            (set-pixel img x y 
              (rgb 
                (/ (extract-red currentPixel) 255)
                (if (< (- (extract-green currentPixel) reductionAmount) 0)
                 0
                 (/ (- (extract-green currentPixel) reductionAmount) 255))
                (/ (extract-blue currentPixel) 255))))
          (recur (inc x))))
      (recur (inc y)))))

(defn reduce-blue [reductionAmount img]
  "Reduces the blue value of an image by the given amount, ranging from 0 to 255"
  (loop [y 0] 
    (when (< y baseHg)
      (loop [x 0]
        (when (< x baseWd)
          (let [currentPixel (get-pixel img x y)] 
            (set-pixel img x y 
              (rgb 
                (/ (extract-red currentPixel) 255)
                (/ (extract-green currentPixel) 255)
                (if (< (- (extract-blue currentPixel) reductionAmount) 0)
                 0
                 (/ (- (extract-blue currentPixel) reductionAmount) 255)))))
          (recur (inc x))))
      (recur (inc y)))))

(defn red-eye [img]
  "Attempts to remove any 'red-eye' effects that may occur in an image by isolating pixels
   with exeptionally high red-levels and reducing them
   This is very much a work in progress. Redeye is a tough issue to isolate"
  (loop [y 0] 
    (when (< y baseHg)
      (loop [x 0]
        (when (< x baseWd)
          (let [currentPixel (get-pixel img x y)] 
              (if (and (> (extract-red currentPixel) 240) (< (extract-green currentPixel) 145) (< (extract-blue currentPixel) 145))
                (do (print x " " y) (set-pixel img x y 
                (rgb 
                  (/ (- (extract-red currentPixel) 100) 255)
                  (/ (extract-green currentPixel) 255)
                  (/ (extract-blue currentPixel) 255))))))
          (recur (inc x))))
      (recur (inc y)))))


(defn get-region-color [x y alph]
  "This is a support method for the pop-art filter
   depending on where the pixel is in the image, a different rgb value is returned" 
  (let [thirdWidth (/ baseWd 3) 
        twoThirdWidth (* 2 thirdWidth) 
        thirdHeight (/ baseHg 3) 
        twoThirdHeight (* 2 thirdHeight)]
    (cond
      (> x twoThirdWidth) (cond
                            (> y twoThirdHeight) (rgb 1 0 1 alph);;fushia
                            (> y thirdHeight) (rgb 0 1 1 alph);;aqua
                            :else (rgb 0 1 0 alph));;lime
      (> x thirdWidth) (cond
                         (> y twoThirdHeight) (rgb 1 1 0 alph);;yellow
                         (> y thirdHeight) (rgb 1 0 0 alph);;red
                         :else (rgb (/ 123 255) (/ 104 255) (/ 238 255) alph));;mediumslateblue
      :else (cond
              (< y thirdHeight) (rgb 1 (/ 165 255) 0 alph);;orange
              (< y twoThirdHeight) (rgb 0 0 1 alph);;blue
              :else (rgb (/ 250 255) (/ 128 255) (/ 114 255) alph));;salmon
      )))

(defn pop-art [img]
  "This is gonna make some rad warhol-esque pop art"
    (loop [y 0] 
      (when (< y baseHg)
        (loop [x 0]
          (when (< x baseWd)
            (let [currentPixel (get-pixel img x y)] 
              (if (> (extract-alpha currentPixel) 0)
                (set-pixel newImg x y (get-region-color x y 1))));;(/ (extract-alpha currentPixel) 255)
            (recur (inc x))))
        (recur (inc y)))))



      


;;(reduce-red 255 baseImg)
;;(reduce-green 255 baseImg)
;;(reduce-blue 255 baseImg)
;;(red-eye baseImg)
;;(show baseImg)

(pop-art baseImg)           
(show newImg)