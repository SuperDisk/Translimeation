(defparameter pointer-table-pos '(#x71174c #x713eb8))

(defmacro comment (&rest rest) nil)
(comment
 (setf *print-vector-length* nil)
 (setf *print-length* 50)
 (defparameter *rom-data* (read-rom "slime.gba"))
 (defparameter *pointer-table-data* (apply #'subseq *rom* pointer-table-pos))
 (defparameter *pointer-table* (parse-pointer-table *pointer-table-data*))
 (defparameter *translation-table* (load-translation-table "SlimeDialogJIS.tbl"))
 )

(defun my-split (string delimiterp)
  (loop :for beg = (position-if-not delimiterp string)
          :then (position-if-not delimiterp string :start (1+ end))
        :for end = (and beg (position-if delimiterp string :start beg))
        :when beg :collect (subseq string beg end)
          :while end))

(defun parse-translation-table-entry (entry)
  (destructuring-bind (hex jchar) (my-split entry (lambda (x) (char= x #\=)))
    (cons (parse-integer hex :radix 16) jchar)))

(defun load-translation-table (table-file)
  (with-open-file (stream table-file :external-format :sjis)
    (loop for line = (read-line stream nil)
          while line
          collect (parse-translation-table-entry line))))

(defun decode-string* (translation-table string)
  (unless (null string)
    (let ((cur (car string))
          (nxt (cadr string))
          mapped-char)
      (cond
        ((and nxt (setf mapped-char (assoc (logior (ash cur 8) nxt) translation-table)))
         (cons (cdr mapped-char) (decode-string* translation-table (cddr string))))
        ((setf mapped-char (assoc cur translation-table))
         (cons (cdr mapped-char) (decode-string* translation-table (cdr string))))
        (t (cons (format nil "($~2,'0X)" cur) (decode-string* translation-table (cdr string))))))))

(defun decode-string (translation-table string)
  (apply #'concatenate 'string (decode-string* translation-table string)))

(defun extract-pointer-table (rom outfile)
  (with-open-file (stream outfile :direction :output
                                  :element-type '(unsigned-byte 8)
                                  :if-exists :supersede)
    (write-sequence (apply #'subseq rom pointer-table-pos) stream)))

(defun extract-strings (rom ptable outfile)
  (with-open-file (stream outfile :direction :output
                                  :element-type '(unsigned-byte 8)
                                  :if-exists :supersede)
    (loop for ptr in (subseq ptable 150)
          do (progn (write-sequence (text-at-pointer rom ptr) stream)))))

(defun text-at-pointer (rom ptr)
  (loop for i = ptr then (1+ i)
        until (= (elt rom i) 0)
        collect (elt rom i)))

(defun nums->pointer (b1 b2 b3 b4)
  (logior (ash b3 16)
          (ash b2 8)
          b1))
(defun parse-pointer-table (table-data)
  (loop for i from 0 below (length table-data) by 4
        collect (nums->pointer (elt table-data (+ i 0))
                               (elt table-data (+ i 1))
                               (elt table-data (+ i 2))
                               (elt table-data (+ i 3)))))

(defun read-rom (rom)
  (with-open-file (stream rom :element-type '(unsigned-byte 8))
    (let ((arr (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence arr stream)
      arr)))
