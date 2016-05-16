;;;; fft-viz.asd
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(asdf:defsystem #:fft-viz
  :description "MP3 based animation stub project."
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC (BSD-like)"
  :depends-on (#:anim-utils
               #:uiop
               #:cl-cairo2
               #:lparallel
               #:mpg123-ffi
               #:bordeaux-fft)
  :serial t
  :components ((:file "package")
               (:file "fft-viz")))

