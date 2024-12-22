(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(cl-ppcre alexandria skippy) :silent t))

(defpackage #:gba2gif
  (:use :cl))
(in-package #:gba2gif)

(defparameter *width* (* 32 8))
(defparameter *height* (* 32 8))

(defun random-color-table ()
  (let ((table (skippy:make-color-table)))
    (loop repeat 255 do
      (skippy:add-color (random #xFFFFFF) table))
    table))

(defparameter *tiles*
  (let ((tile-data (alexandria:read-file-into-byte-vector "./map-recreation/gfx.bin")))
    (loop for i from 0 below (length tile-data) by 32
          collect
          (skippy:make-canvas :width 8
                              :height 8
                              :image-data
                              (coerce
                               (loop for el across (subseq tile-data i (+ i 32))
                                     append
                                     (list (ldb (byte 4 0) el) (ldb (byte 4 4) el)))
                               '(vector (unsigned-byte 8)))))))

(defparameter *map*
  (let ((bytes (alexandria:read-file-into-byte-vector "./map-recreation/map.bin")))
    (loop for (b1 b2) on (coerce bytes 'list) by #'cddr
          collect (logand #b1111111111 (logior b1 (ash b2 8))))))


(defparameter *image* (skippy:make-image :width (* 32 8)
                                         :height (* 32 8)))

(loop for tile in *map*
      for idx from 0 by 8
      for x = (mod idx *width*)
      for y = (* (floor idx *height*) 8)
      do (skippy:composite (nth tile *tiles*) *image*
                           :sx 0 :sy 0
                           :dx x :dy y))

(defun dump-img (img path)
  (let ((ds (skippy:make-data-stream :width *width*
                                     :height *height*
                                     :color-table (random-color-table)
                                     :initial-images (list (skippy:canvas-image img)))))
    (skippy:output-data-stream ds path)))
