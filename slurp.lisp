(defparameter pointer-table-pos '(#x71174c #x713ebc))

(defmacro comment (&rest rest) nil)
(comment
 (defparameter *ibm-api-endpoint* "https://api.us-south.language-translator.watson.cloud.ibm.com/instances/63165d7f-1133-4eff-94bd-30c445d34bba/v3/translate?version=2018-05-01")
 (defparameter *ibm-api-key*
   (with-open-file (stream "api-key.txt")
     (read stream)))

 (defun jswrap (txt)
   (cl-json:encode-json-to-string `(("text" . ,(list txt)) ("model_id" . "ja-en"))))

 (defun translate-fragment (frag)
   (when (and (> (length frag) 1) (not (zerop (mismatch "『" frag))))
       (setf frag (subseq frag 1)))
   (cdr
    (assoc :translation
           (cadr
            (assoc
             :translations
             (cl-json:decode-json
              (drakma:http-request *ibm-api-endpoint* :method :post
                                                      :content-type "application/json"
                                                      :basic-authorization (list "apikey" *ibm-api-key*)
                                                      :content (jswrap frag)
                                                      :want-stream t)))))))

 (defun translate-string (string)
   (loop for element in string collect
     (if (stringp element)
         (translate-fragment element)
         element)))

 (drakma:http-request *ibm-api-endpoint* :method :post
                                         :basic-authorization (list "apikey" *ibm-api-key*)
                                         :content (jswrap "そんなことして だいじょうぶかな?"))

 (defparameter reformd9 (mapcar (lambda (x) (hack-reformat (cons (car x) (reflow-string (cdr x))))) *all-text-translated*))
 (defparameter slime-patched (read-rom "slime_original.gba"))
 (patch-rom slime-patched *inv-translation-table* reformd9)
 (dump-rom slime-patched "translated10.gba")

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
           do (progn (write-sequence (string-at-pointer rom ptr) stream)))))
 (defun dump-all-text (fname txts)
   (progn
     (with-open-file (stream fname :direction :output
                                   :external-format :sjis)
       (with-standard-io-syntax
         (loop for txt in txts do
           (prin1 txt stream)
           (terpri stream))))
     nil))
 (defun dump-all-text-utf (fname txts)
   (progn
     (with-open-file (stream fname :direction :output
                                   :external-format :utf-8)
       (with-standard-io-syntax
         (loop for txt in txts do
           (prin1 txt stream)
           (terpri stream))))
     nil))

 (defparameter *all-text-translated* nil)
 (loop for string in *all-texts* do
   (let ((translated (translate-string string)))
     (print translated)
     (push translated *all-text-translated*)))

 (defun hack-reformat (x)
   (cons (car x) (list (cdr x))))

 (defun reflow-string (string)
   (setf string (remove-if (lambda (x) (equal x '(byte 2))) string))
   (let (output (i 0))
     (loop for elm in string
           finally (return (reverse output)) do
       (cond
         ((stringp elm)
          (let ((str-build ""))
            (loop for char across elm do
              (setf str-build (concatenate 'string str-build (list char)))
              (when (>= i 21)
                (setf i 0)
                (push str-build output)
                (push '(byte 2) output)
                (setf str-build ""))
              (incf i))
            (when (not (string= str-build ""))
              (push str-build output))))
         (t (when (equal elm '(byte 5))
            (setf i 0))
          (push elm output))))))

 (setf *print-vector-length* nil)
 (setf *print-length* 50)
 (defparameter *rom-data* (read-rom "slime.gba"))
 (defparameter *pointer-table-data* (apply #'subseq *rom-data* pointer-table-pos))
 (defparameter *pointer-table* (parse-pointer-table *pointer-table-data*))
 (defparameter *pointer-table2* (subseq *pointer-table* 53 2299))
 (defparameter *translation-table* (reverse (load-translation-table "SlimeDialogJIS.tbl")))
 (defparameter *all-texts*
   (mapcar (lambda (x)
             (cons (car x) (decode-string *translation-table* (cdr x) *rom-data*)))
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

(defun decode-string (translation-table offset rom &optional (cur-str ""))
  (let ((cur (elt rom offset))
        (nxt (elt rom (1+ offset)))
        mapped-char)
    (cond
      ((zerop cur) (if (string= cur-str "") nil (list cur-str)))
      ((setf mapped-char (assoc (logior (ash cur 8) nxt) translation-table))
       (decode-string translation-table (+ 2 offset) rom (concatenate 'string cur-str (cdr mapped-char))))
      ((setf mapped-char (assoc cur translation-table))
       (decode-string translation-table (1+ offset) rom (concatenate 'string cur-str (cdr mapped-char))))
      ((not (string= cur-str "")) (list* cur-str (decode-string translation-table offset rom "")))
      ((= cur #x0c) ; color codes
       (cons `(color ,nxt) (decode-string translation-table (+ 2 offset) rom cur-str)))
      (t (cons `(byte ,cur) (decode-string translation-table (1+ offset) rom cur-str))))))
(defun encode-string (inv-translation-table string)
  (flet ((encode-str (string)
           (loop for char across string
                 for encoded = (assoc char inv-translation-table :test #'string=)
                 collect (if encoded (cdr encoded) 94))))
    (cond
      ((null string) nil)
      ((stringp (car string)) (append (encode-str (car string)) (encode-string inv-translation-table (cdr string))))
      ((equal (caar string) 'color) (list* #x0c (cadar string) (encode-string inv-translation-table (cdr string))))
      ((equal (caar string) 'byte) (cons (cadar string) (encode-string inv-translation-table (cdr string)))))))

(defun parse-pointer-table (table-data)
  (flet ((nums->pointer (b1 b2 b3 b4)
           (logior (ash b3 16)
                   (ash b2 8)
                   b1)))
    (loop for i from 0 below (length table-data) by 4
          for j = 0 then (1+ j)
          collect (cons j (nums->pointer (elt table-data (+ i 0))
                                         (elt table-data (+ i 1))
                                         (elt table-data (+ i 2))
                                         (elt table-data (+ i 3)))))))

(defun patch-rom (rom encoding-table new-strings)
  (flet ((insert-string (bytes)
           (loop for byte in bytes do
             (cond
               ((= byte #x010c)
                (vector-push #x01 rom)
                (vector-push #x0c rom))
               ((= byte #x010b)
                (vector-push #x01 rom)
                (vector-push #x0c rom))
               (t (vector-push byte rom))))
           (vector-push 0 rom))) ; null terminator
    (let ((pointer-patches nil))
      (loop for (table-index string) in new-strings do
        (push (cons table-index (fill-pointer rom)) pointer-patches)
        (insert-string (encode-string encoding-table string)))
      (loop for (table-index . pointer) in pointer-patches do
        (let* ((tbl-pos (+ (car pointer-table-pos) (* 4 table-index)))
               (table-entry (logior #x8000000 pointer)))
          (loop for i from 0 to 24 by 8
                for bt = (ldb (byte 8 i) table-entry)
                for pos from tbl-pos do
                  (setf (aref rom pos) bt)))))))

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
