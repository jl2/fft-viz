;;;; fft-viz.lisp
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:fft-viz)

(defun cairo-line (x1 y1 x2 y2)
  "Use cairo to draw a line from point (x1 y1) to (x2 y2)."
  (cl-cairo2:move-to x1 y1)
  (cl-cairo2:line-to x2 y2)
  (cl-cairo2:stroke))

(defun generate-frame (left-fft-data right-fft-data)
  "Create a data structure that represents one frame of the animation.
   Later, the frame data is used to generate the actual image."
  (cons (abs (aref left-fft-data 0)) (abs (aref right-fft-data 0))))

(defun draw-frame (file-name frame width height)
  "Use cairo to draw a frame of the animation, saving in the specified file."
  (cl-cairo2:with-png-file (file-name :argb32 width height)

    ;; Basic black background
    (cl-cairo2:set-source-rgba 0.0 0.0 0.0 1.0)
    (cl-cairo2:paint)
    
    (cl-cairo2:scale 1 1)
    (cl-cairo2:set-line-width 1.25)
    (cl-cairo2:set-source-rgba 0.0 0.8 0.0 0.95)

    (let ((x-aspect-ratio (if (< height width)
                              (/ height width 1.0)
                              1.0))
          (y-aspect-ratio (if (< height width)
                              1.0
                              (/ width height 1.0))))
    
      ;; Local functions for mapping logical coordinates to physical coordinates
      (flet (
             ;; xmapper maps logical x coordinates in the range x-min to x-max to
             ;; screen coordinates in the range 0 to width
             (xmapper (x) (map-val (* x-aspect-ratio x) -200.0 200.0 0 width))

             ;; ymapper does the same thing, but for y coordinates
             (ymapper (y) (map-val (* y-aspect-ratio y) -200.0 200.0 0 height)))

        ;; Actual drawing goes here.  In this case, just a line.
        (cairo-line (xmapper (- (car frame))) (ymapper 0.0)
                    (xmapper (cdr frame)) (ymapper 0.0))))))

(defun from-mp3 (&key
                   mp3-file-name output-directory
                   (movie-file-name "mp3-animation.mpg")
                   (keep-pngs nil)
                   (keep-soundless nil)
                   (width 800) (height 800)
                   (bit-rate (* 4 1024))
                   (fps 30)
                   (verbose t)
                   (threads 4)
                   (fft-window-size 1024)
                   (movie-duration nil)
                   (temp-movie-name "temporary.mpg"))
  "Generate an animation from an MP3 file."
  
  (let* ((real-dir-name (fix-directory output-directory))

         (mp3-file (read-mp3-file mp3-file-name))

         (song-duration (mp3-file-duration-in-seconds mp3-file))
         (real-movie-duration (if movie-duration
                                  (min song-duration movie-duration)
                                  song-duration))
         
         (total-frames (ceiling (* real-movie-duration fps)))

         (files-created nil)
         (frames nil)
         (full-movie-name (format nil "~a~a" real-dir-name movie-file-name))
         (full-temp-movie-name (format nil "~a~a" real-dir-name temp-movie-name))

         (kernel (lparallel:make-kernel threads))
         (futures nil))
    
    (when verbose (format t "Creating animation with ~a frames." total-frames))

    (dotimes (cur-frame total-frames)
      (let* ((file-name (format nil
                                "~aframe~5,'0d.png" real-dir-name cur-frame))
             
             (win-center (ceiling (* 44100 (interpolate 0.0 song-duration
                                                        cur-frame total-frames))))
             (left-fft-data (bordeaux-fft:windowed-fft (mp3-file-left-channel mp3-file) win-center fft-window-size))
             (right-fft-data (bordeaux-fft:windowed-fft (mp3-file-right-channel mp3-file) win-center fft-window-size)))

        

        ;; (when verbose (format t "~a~%" spiro))
        (push (cons (generate-frame left-fft-data right-fft-data) file-name) frames)
        (push file-name files-created)))

    (setf lparallel:*kernel* kernel)
    (unwind-protect

         (dolist (frame frames)
           (push (lparallel:future
                   ;; (format t "Drawing ~a~%" harm)
                   (draw-frame (cdr frame) (car frame) width height)
                   (push (cdr frame) files-created))
                 futures))
      (when futures (dolist (fut futures) (lparallel:force fut)))
      (when kernel (lparallel:end-kernel :wait t)))

    (make-movie :directory real-dir-name
                :image-type "png"
                :mp3-name mp3-file-name
                :file-name full-movie-name
                :remove-temp (not keep-soundless) 
                :temp-name full-temp-movie-name
                :bit-rate bit-rate)
  
    (if (not keep-pngs)
        (dolist (fname files-created)
          (if (probe-file fname)
              (delete-file fname))))))

