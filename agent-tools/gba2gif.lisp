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

(defun greyscale-color-table ()
  (let ((table (skippy:make-color-table)))
    (loop for i below 16
          for val = (floor (* 255 (/ i 16))) do
      (skippy:add-color (logior val (ash val 8) (ash val 16)) table))
    table))

(defun load-tiles (path)
  (let ((tile-data (alexandria:read-file-into-byte-vector path)))
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

(defun load-map (path)
  (let ((bytes (alexandria:read-file-into-byte-vector path)))
    (loop for (b1 b2) on (coerce bytes 'list) by #'cddr
          collect (logand #b1111111111 (logior b1 (ash b2 8))))))

(defun convert (tiles-path map-path)
  (let ((image (skippy:make-image :width *width*
                                  :height *height*))
        (tiles (load-tiles tiles-path))
        (map (load-map map-path)))
    (loop for tile in map
          for idx from 0 by 8
          for x = (mod idx *width*)
          for y = (* (floor idx *height*) 8)
          do (skippy:composite (nth tile tiles) image
                               :sx 0 :sy 0
                               :dx x :dy y))
    image))

(defun preview-gfx (path)
  (let ((gfx (load-tiles path))
        (mapsize 0))
    (loop while (< (* mapsize mapsize) (length gfx)) do
      (format t "~a ~a~%" (* mapsize mapsize) (length gfx))
      (incf mapsize))
    (format t "final mapsize ~a~%" mapsize)
    (let ((img (skippy:make-image :width (* mapsize 8)
                                  :height (* mapsize 8))))
      (loop for tile in gfx
            for idx from 0 by 8
            for x = (mod idx (* mapsize 8))
            for y = (* 8 (floor idx (* mapsize 8))) do
              (format t "placing at ~a ~a ~%" x y)
        (skippy:composite tile img
                          :sx 0 :sy 0
                          :dx x :dy y))
      img)))

(dump-img (preview-gfx "/home/npfaro/projects/Translimeation/BizHawk-2.10-rc2-linux/slime_graphics/81DAC74.bin") "/tmp/somecrap.gif")

(defun dump-img (img path)
  (let ((ds (skippy:make-data-stream :width (skippy:width img)
                                     :height (skippy:height img)
                                     :color-table (greyscale-color-table)
                                     :initial-images (list (skippy:canvas-image img)))))
    (skippy:output-data-stream ds path)))


(defparameter bins
  (mapcar #'namestring
          (uiop/filesystem:directory-files "/home/npfaro/projects/Translimeation/BizHawk-2.10-rc2-linux/slime_graphics/")))

(defparameter gfxbase "8558BCC.bin")

(defparameter pth "/home/npfaro/projects/Translimeation/BizHawk-2.10-rc2-linux/slime_graphics/")

(defun str+ (&rest strings)
  (apply #'concatenate 'string strings))

(loop for bin in bins do
  (ignore-errors
    (dump-img (preview-gfx bin) (format nil "/tmp/gifs/~a.gif" (gensym)))
   #+nil(dump-img (convert (str+ pth gfxbase) (str+ pth bin))
             (format nil "/tmp/~a.gif" (gensym)))))
