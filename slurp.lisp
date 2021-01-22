(defparameter pointer-table-pos '(#x71174c #x713eb8))

(defmacro comment (&rest rest) nil)
(comment
 (setf *print-vector-length* nil)
 (setf *print-length* 50)
 (defparameter *rom-data* (read-rom "slime.gba"))
 (defparameter *pointer-table-data* (apply #'subseq *rom-data* pointer-table-pos))
 (defparameter *pointer-table* (parse-pointer-table *pointer-table-data*))
 (defparameter *pointer-table2* (subseq *pointer-table* 53 2299))
 (defparameter *translation-table* (reverse (load-translation-table "SlimeDialogJIS.tbl")))
 (defparameter *all-texts*
   (mapcar (lambda (x)
             (cons (car x) (decode-string *translation-table* (text-at-pointer *rom-data* (cdr x)))))
           *pointer-table2*))
 )

(defun invert-alist (alist)
  (loop for (a . b) in alist
        collect (cons b a)))

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

(defun decode-string (translation-table string &optional (cur-str ""))
  (if (null string) (if (string= cur-str "") nil (list cur-str))
    (let ((cur (car string))
          (nxt (cadr string))
          mapped-char)
      (cond
        ((and nxt (setf mapped-char (assoc (logior (ash cur 8) nxt) translation-table)))
         (decode-string translation-table (cddr string) (concatenate 'string cur-str (cdr mapped-char))))
        ((setf mapped-char (assoc cur translation-table))
         (decode-string translation-table (cdr string) (concatenate 'string cur-str (cdr mapped-char))))
        ((string= cur-str "") (cons `(byte ,cur) (decode-string translation-table (cdr string))))
        (t (list* cur-str `(byte ,cur) (decode-string translation-table (cdr string))))))))

(defun encode-string (inv-translation-table string)
  (flet ((encode-str (string)
           (loop for char across string
                 collect (cdr (assoc char inv-translation-table :test #'string=)))))
    (cond
      ((null string) '(0))
      ((stringp (car string)) (append (encode-str (car string)) (encode-string inv-translation-table (cdr string))))
      ((equal (caar string) 'byte) (cons (cadar string) (encode-string inv-translation-table (cdr string)))))))

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
  (if (= ptr 15214352)
      '(1 1 1 1) ;hack
      (loop for i = ptr then (1+ i)
            until (= (elt rom i) 0)
            collect (elt rom i))))

(defun nums->pointer (b1 b2 b3 b4)
  (logior (ash b3 16)
          (ash b2 8)
          b1))
(defun parse-pointer-table (table-data)
  (loop for i from 0 below (length table-data) by 4
        for j = 0 then (1+ j)
        collect (cons j (nums->pointer (elt table-data (+ i 0))
                                       (elt table-data (+ i 1))
                                       (elt table-data (+ i 2))
                                       (elt table-data (+ i 3))))))

(defun read-rom (rom &optional (expansion #x100000)) ;expand by 1MB
  (with-open-file (stream rom :element-type '(unsigned-byte 8))
    (let ((arr (make-array (+ (file-length stream) expansion)
                           :element-type '(unsigned-byte 8)
                           :fill-pointer (file-length stream))))
      (read-sequence arr stream)
      arr)))

(defun dump-rom (rom file)
  (with-open-file (stream file
                          :element-type '(unsigned-byte 8)
                          :direction :output
                          :if-exists :supersede)
    (write-sequence rom stream)))
