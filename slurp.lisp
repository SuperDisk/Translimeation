(ql:quickload 'cl-json)
(ql:quickload 'drakma)

;; Text opcodes

;; 02 = newline
;; 07 08 = wait for new line?
;; 00 = null terminator
;; 0c = color codes (00 = normal)
;; 0e = player name
;; 0b = move textbox to bottom??

(defparameter pointer-table-pos '(#x71174c #x713CC4))
(defparameter *letter-sizes*
  '((#\A . 9) (#\B . 8) (#\C . 9) (#\D . 8)
    (#\E . 7) (#\F . 7) (#\G . 9) (#\H . 8)
    (#\I . 6) (#\J . 7) (#\K . 8) (#\L . 7)
    (#\M . 9) (#\N . 8) (#\O . 10) (#\P . 7)
    (#\Q . 10) (#\R . 8) (#\S . 7) (#\T . 8)
    (#\U . 8) (#\V . 9) (#\W . 11) (#\X . 8)
    (#\Y . 9) (#\Z . 7) (#\a . 8) (#\b . 8)
    (#\c . 8) (#\d . 8) (#\e . 8) (#\f . 7)
    (#\g . 8) (#\h . 7) (#\i . 4) (#\j . 6)
    (#\k . 7) (#\l . 6) (#\m . 10) (#\n . 8)
    (#\o . 9) (#\p . 8) (#\q . 8) (#\r . 6)
    (#\s . 7) (#\t . 6) (#\u . 8) (#\v . 9)
    (#\w . 11) (#\x . 8) (#\y . 8) (#\z . 7)
    (#\Space . 1)))
(defparameter *textbox-size* 206)

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

 (translate-fragment "そんなことして だいじょうぶかな?")

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

 (defun old-reflow-string (string)
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
 (defparameter *rom-data* (read-rom "slime_original.gba"))
 (defparameter *pointer-table-data* (apply #'subseq *rom-data* pointer-table-pos))
 (defparameter *pointer-table* (parse-pointer-table *pointer-table-data*))
 (defparameter *pointer-table2* (subseq *pointer-table* 53 2299))
 (defparameter *translation-table* (reverse (load-translation-table "SlimeDialogJIS.tbl")))
 (defparameter *small-translation-table* (reverse (load-translation-table "Slime_SmallJIS.tbl")))
 (defparameter *ignore* '(1982 1895 1897 1900 50))
 (defparameter *all-texts*
   (handler-bind ((malformed-string-error #'skip-malformed-string))
     (load-all-texts *rom-data* *pointer-table* *translation-table* *small-translation-table* *ignore*)))

 (defun find-ptr (x)
   (+ (car pointer-table-pos) (* 4 x))))

(define-condition malformed-string-error (error)
  ((text :initarg :text :reader text)))
(defun skip-malformed-string (c)
  (invoke-restart 'skip-malformed-string))

(defun load-all-texts (rom pointer-table translation-table small-translation-table &optional (ignore nil))
  (loop for el in (remove-if (lambda (x) (member (car x) ignore)) pointer-table)
        for (idx . addr) = el
        for entry = (restart-case (decode-string translation-table small-translation-table addr rom)
                      (skip-malformed-string ()
                        (format t "Malformed string at table idx ~a: ~a~%" idx addr)
                        nil))
        when entry collect (cons idx entry)))

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

(defun remove-reflow-opcodes (string)
  (let ((stripped (remove-if (lambda (x)
                               (member x '((newline) (byte #x07) (byte #x08)) :test #'equal))
                             string)))
    (labels ((join-strs (ls &optional (cur-str ""))
               (cond
                 ((null ls) (if (string= cur-str "") nil (list cur-str)))
                 ((stringp (car ls)) (join-strs (cdr ls) (concatenate 'string cur-str (car ls))))
                 ((not (string= cur-str "")) (list* cur-str (car ls) (join-strs (cdr ls))))
                 (t (cons (car ls) (join-strs (cdr ls)))))))
      (join-strs stripped))))
(defun reflow-string (str)
  (flet ((word-width (word)
           (loop for char across word
                 sum (or (cdr (assoc char *letter-sizes*)) 6))))
    (let ((out nil)
          (cur-str "")
          (pixels 0)
          (need-wait nil))
      (loop for elem in str do
        (if (not (stringp elem))
            (progn
              (when (not (string= cur-str ""))
                (push cur-str out))
              (push elem out)
              (setf cur-str ""))
            (let ((words (my-split elem (lambda (x) (string= x " ")))))
              (loop for word in words do
                (progn
                  (incf pixels (1+ (word-width word)))
                  (when (>= pixels *textbox-size*)
                    (when (not (string= cur-str ""))
                      (push cur-str out))
                    (when need-wait
                      (push '(byte 7) out)
                      (push '(byte 8) out))
                    (setf need-wait (not need-wait))
                    (push '(newline) out)
                    (setf pixels 0)
                    (setf cur-str ""))
                  (setf cur-str
                        (concatenate 'string
                                     cur-str
                                     (when (not (string= cur-str "")) " ")
                                     word))))
              (when (not (string= cur-str ""))
                (push cur-str out)))))
      (when (equal (car out) '(newline))
        (setf out (cdr out)))
      (reverse out))))

(defun decode-small-string (translation-table small-translation-table offset rom &optional (cur-str ""))
  (let* ((cur (elt rom offset))
         (translated (assoc cur small-translation-table)))
    (cond
      ((= cur #x05)
       (cons (list 'name cur-str) (decode-string translation-table small-translation-table (1+ offset) rom)))
      ((not translated)
       (error 'malformed-string-error :text "yeah")
       nil)
      (t (decode-small-string translation-table small-translation-table (1+ offset) rom (concatenate 'string cur-str (cdr translated)))))))
(defun decode-string (translation-table small-translation-table offset rom &optional (cur-str ""))
  (let ((cur (elt rom offset))
        (nxt (elt rom (1+ offset)))
        mapped-char)
    (cond
      ((zerop cur) (if (string= cur-str "") nil (list cur-str)))
      ((setf mapped-char (assoc (logior (ash cur 8) nxt) translation-table))
       (decode-string translation-table small-translation-table (+ 2 offset) rom (concatenate 'string cur-str (cdr mapped-char))))
      ((setf mapped-char (assoc cur translation-table))
       (decode-string translation-table small-translation-table (1+ offset) rom (concatenate 'string cur-str (cdr mapped-char))))
      ((not (string= cur-str "")) (list* cur-str (decode-string translation-table small-translation-table offset rom "")))
      ((= cur #x05) ; name
       (decode-small-string translation-table small-translation-table (1+ offset) rom))
      ((= cur #x0c) ; color codes
       (cons `(color ,nxt) (decode-string translation-table small-translation-table (+ 2 offset) rom cur-str)))
      ((= cur #x02) ; new line
       (cons '(newline) (decode-string translation-table small-translation-table (1+ offset) rom cur-str)))
      ((= cur #x0e) ; player name
       (cons '(player-name) (decode-string translation-table small-translation-table (1+ offset) rom cur-str)))
      (t (cons `(byte ,cur) (decode-string translation-table small-translation-table (1+ offset) rom cur-str))))))
(defun encode-string (inv-translation-table inv-translation-table-small string)
  (flet ((encode-str (string tbl)
           (loop for char across string
                 for encoded = (assoc char tbl :test #'string=)
                 collect (if encoded (cdr encoded) 94))))
    (cond
      ((null string) nil)
      ((stringp (car string))
       (append
        (encode-str (car string) inv-translation-table)
        (encode-string inv-translation-table inv-translation-table-small (cdr string))))
      ((equal (caar string) 'newline)
       (cons #x02 (encode-string inv-translation-table inv-translation-table-small (cdr string))))
      ((equal (caar string) 'name)
       (append '(#x05)
               (encode-str (cadar string) inv-translation-table-small)
               '(#x05)
               (encode-string inv-translation-table inv-translation-table-small (cdr string))))
      ((equal (caar string) 'color)
       (list* #x0c
              (cadar string)
              (encode-string inv-translation-table inv-translation-table-small (cdr string))))
      ((equal (caar string) 'player-name)
       (cons #x0e (encode-string inv-translation-table inv-translation-table-small (cdr string))))
      ((equal (caar string) 'byte)
       (cons (cadar string) (encode-string inv-translation-table inv-translation-table-small (cdr string))))))))
(defun verify-isomorphic (string tt tts)
  (let* ((encoded (encode-string (invert-alist tt) (invert-alist tts) string))
         (decoded (decode-string tt tts 0 (append encoded '(0 0)))))
    (if (not (equal decoded string))
        string)))

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
               ((= byte #x010c) ; temporary hack to allow insertion of multi-byte character (W,w)
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
