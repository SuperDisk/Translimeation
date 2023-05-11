(ql:quickload 'cl-json)
(ql:quickload 'drakma)
(ql:quickload 'pithy-xml)

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
    (#\Space . 6) (#\~ . 9) (#\. . 2) (#\, . 4)
    (#\" . 5) (#\' . 6) (#\! . 4) (#\? . 9)
    (#\) . 6) (#\( . 6)
    (#\0 . 7) (#\1 . 7) (#\2 . 7) (#\3 . 7) (#\4 . 7)
    (#\5 . 7) (#\6 . 7) (#\7 . 7) (#\8 . 7) (#\9 . 7)))
(defparameter *textbox-size* 208)

(defun get-in (indices list)
  (loop for index in indices do
    (cond
      ((numberp index) (setf list (elt list index)))
      (t (setf list (cdr (assoc index list)))))
        finally (return list)))

#+nil
(progn
  (defparameter *ibm-api-endpoint* "https://api.us-south.language-translator.watson.cloud.ibm.com/instances/63165d7f-1133-4eff-94bd-30c445d34bba/v3/translate?version=2018-05-01")
  (defparameter *ibm-api-key*
    (with-open-file (stream "ibm-api-key.txt")
      (read stream)))

  (defparameter *deepl-api-endpoint* "https://api.deepl.com/v2/translate")
  (defparameter *deepl-api-key*
    (with-open-file (stream "deepl-api-key.txt")
      (read stream)))

  (defparameter *openai-api-endpoint* "https://api.openai.com/v1/chat/completions")
  (defparameter *openai-api-key*
    (with-open-file (stream "openai-api-key.txt")
      (read stream)))
  (defparameter *openai-prompt* "You are a Japanese to English translator. You will receive S-expressions which contain strings of Japanese text, which you will translate into English. The text may be split into multiple strings, but this does not indicate any split between sentences or any other kind of delimiting-- treat the strings as if they were one continuous one.")

  (defun translate-fragment-ibm (frag)
    (setf frag (remove #\『 frag)) ;hack
    (if (string= frag "")
        ""
        (cdr
         (assoc :translation
                (cadr
                 (assoc
                  :translations
                  (cl-json:decode-json
                   (drakma:http-request *ibm-api-endpoint* :method :post
                                                           :content-type "application/json"
                                                           :basic-authorization (list "apikey" *ibm-api-key*)
                                                           :content (cl-json:encode-json-to-string
                                                                     `(("text" . (,frag))
                                                                       ("model_id" . "ja-en")))
                                                           :want-stream t))))))))
  (defun translate-fragment-deepl (frag)
    (setf frag (remove #\『 frag)) ;hack
    (if (string= frag "")
        ""
        (cdr
         (assoc :text
                (cadr
                 (assoc
                  :translations
                  (cl-json:decode-json
                   (drakma:http-request *deepl-api-endpoint*
                                        :external-format-out :utf-8
                                        :method :post
                                        :parameters `(("auth_key" . ,*deepl-api-key*)
                                                      ("text" . ,frag)
                                                      ("target_lang" . "EN"))
                                        :want-stream t))))))))

  (defun translate-gpt4 (string)
    (read-from-string
     (get-in '(:choices 0 :message :content)
             (cl-json:decode-json
              (drakma:http-request *openai-api-endpoint*
                                   :method :post
                                   :content-type "application/json"
                                   :additional-headers `(("Authorization" . ,(format nil "Bearer ~a" *openai-api-key*)))
                                   :want-stream t
                                   :content (cl-json:encode-json-to-string
                                             `(("model" . "gpt-4")
                                               ("messages" . ((("role" . "system")
                                                               ("content" . ,*openai-prompt*))
                                                              (("role" . "user")
                                                               ("content" . ,(write-to-string string))))))))))))

  (defun translate-string (string)
    (loop for element in string
          collect
          (cond
            ((stringp element)
             (translate-fragment-deepl element))
            ((and (consp element) (equal (car element) 'name))
             (list (car element) (translate-fragment-deepl (cadr element))))
            (t element))))

  (translate-fragment-deepl "そんなことして だいじょうぶかな?")
  (translate-fragment-ibm "そんなことして だいじょうぶかな?")

  (defun dump-all-text-utf (fname txts)
    (progn
      (with-open-file (stream fname :direction :output
                                    :external-format :utf-8
                                    :if-exists :supersede)
        (with-standard-io-syntax
          (loop for txt in txts do
            (prin1 txt stream)
            (terpri stream))))
      nil))

  (defun read-all-text-utf (fname)
    (with-open-file (stream fname :direction :input
                                  :external-format :utf-8)
      (loop for sexp = (read stream nil)
            while sexp
            collect sexp)))

  (defparameter *all-text-translated* nil)

  ;; note: something special about text 1403 --- might be ducktor cid's crashing line

  ;; This block of code isn't wrapped in a function so that if the process gets interrupted, we can still
  ;; salvage the already-processed translated texts, as running everything through the API takes a while.
  (loop for string in *all-texts* do
    (let* ((stripped (remove-reflow-opcodes string))
           (translated (translate-string stripped)))
      (print translated)
      (push translated *all-text-translated*)))

  (defun trans (f)
    (let ((slime-patched (read-rom "slime_original.gba")))
      (patch-rom slime-patched
                 (invert-alist *translation-table*)
                 (invert-alist *small-translation-table*)
                 (mapcar #'reflow-string *all-text-translated*))
      (dump-rom slime-patched f)))

  ;; All the necessary data to create *all-texts*, which is the useful bit of data we need
  ;; to create *all-text-translated*, which is injected back into the ROM.
  (defparameter *rom-data* (read-rom "slime_original.gba"))
  (defparameter *pointer-table-data* (apply #'subseq *rom-data* pointer-table-pos))
  (defparameter *pointer-table* (parse-pointer-table *pointer-table-data*))
  (defparameter *pointer-table2* (subseq *pointer-table* 53 2299))
  (defparameter *translation-table* (reverse (load-translation-table "SlimeDialogJIS.tbl")))
  (defparameter *small-translation-table* (reverse (load-translation-table "Slime_SmallJIS.tbl")))
  (defparameter *ignore* (append (range 52) '(1892 1895 1897 1900)))
  (defparameter *all-texts*
    (handler-bind ((malformed-string-error #'skip-malformed-string))
      (load-all-texts *rom-data* *pointer-table* *translation-table* *small-translation-table* *ignore*)))

  )

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

(defun range (max &key (min 0) (step 1))
  (loop for n from min below max by step
        collect n))
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
                 sum (let ((width (cdr (assoc char *letter-sizes*))))
                       (if width (1+ width) 11)))))
    (let ((out nil)
          (cur-str "")
          (pixels 0)
          (need-wait nil))
      (loop for elem in str do
        (cond
          ((stringp elem)
           (let ((words (my-split elem (lambda (x) (string= x " ")))))
             (loop for word in words do
               (progn
                 (incf pixels (word-width word))
                                        ;(format t "~a ; word-width = ~a ; pixels = ~a~%" word (word-width word) pixels)
                 (when (< pixels *textbox-size*)
                   (incf pixels (cdr (assoc #\Space *letter-sizes*))))
                 (when (>= pixels *textbox-size*)
                                        ;(format t "Splitting before ~a, ~a >= ~a~%" word pixels *textbox-size*)
                   (when (not (string= cur-str ""))
                     (push cur-str out))
                   (when need-wait
                     (push '(byte 7) out)
                     (push '(byte 8) out))
                   (setf need-wait (not need-wait))
                   (push '(newline) out)
                   (setf pixels (word-width word))
                   (setf cur-str ""))
                 (setf cur-str
                       (concatenate 'string
                                    cur-str
                                    (when (not (string= cur-str "")) " ")
                                    word))))
             (when (not (string= cur-str ""))
               (push cur-str out))))
          ((equal elem '(player-name))
           (push elem out)
           (setf cur-str "")
           (incf pixels (word-width "WWWW"))) ; hack: assume 4 widest characters
          (t
           (push elem out)
           (setf cur-str ""))))
      (when (equal (car out) '(newline))
        (setf out (cdr out)))
      (when (not (equal (car out) '(byte 8)))
        (push '(byte 7) out)
        (push '(byte 8) out))
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
                 for encoded = (or (assoc char tbl :test #'string=)
                                   (assoc "?" tbl :test #'string=))
                 collect (cdr encoded))))
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
       (cons (cadar string) (encode-string inv-translation-table inv-translation-table-small (cdr string)))))))
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

(defun patch-rom (rom encoding-table small-encoding-table new-strings)
  (flet ((insert-string (bytes)
           (loop for byte in bytes do
             (cond
               ((= byte #x010c) ; temporary hack to allow insertion of multi-byte character (W,w)
                (vector-push #x01 rom)
                (vector-push #x0c rom))
               ((= byte #x010b)
                (vector-push #x01 rom)
                (vector-push #x0b rom))
               (t (vector-push byte rom))))
           (vector-push 0 rom))) ; null terminator
    (let ((pointer-patches nil))
      (loop for (table-index . string) in new-strings do
        (push (cons table-index (fill-pointer rom)) pointer-patches)
        (insert-string (encode-string encoding-table small-encoding-table string)))
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
    (write-sequence rom stream)
    nil))
