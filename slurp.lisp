(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf))

(defpackage #:slurp
  (:use :cl))
(in-package #:slurp)

;; Verified against 08095F34 and its dispatch/continuation tables.
;; 00 ends the stream; 01 prefixes a two-byte glyph; 05 delimits a small-font name.
;; The plain/menu renderer and credits use DIFFERENT grammars; see tools/text_codec.py.
(defparameter *text-opcodes*
  '((newline 2 0) (scroll 3 0) (clear 4 0) (delay 6 1)
    (show-prompt 7 0) (wait-input 8 0) (yes-no 9 0) (open-menu 10 1)
    (switch-window 11 0) (color 12 1) (dynamic-text 13 1)
    (player-name 14 0) (nop 15 0)))

(defparameter txt nil)
(defun load-texts ()
  (setf txt (read-dialogue-text "text-dumps/gerb.txt")))

(defparameter pointer-table-pos '(#x71174c #x713CC4))
(defparameter *font-records* nil)
;; 080971EC selects one of ten records at ROM offset 713EB8.
;; Each record is {u32 bitmap-pointer, u16 first-code, u8 width, u8 stride}.
(defparameter *font-range-ends* '(#x13 #x18 #x23 #x3d #x66 #x97 #xce #x10c #x141 #x1b8))

(defun load-font-metrics (&optional (rom (read-rom "slime_original.gba")))
  (setf *font-records*
        (loop for pos from #x713eb8 by 8
              for end in *font-range-ends*
              collect (list (logior (aref rom (+ pos 4))
                                   (ash (aref rom (+ pos 5)) 8))
                            end (aref rom (+ pos 6))))))

(defun glyph-width (code)
  (unless *font-records* (load-font-metrics))
  (or (loop for (start end width) in *font-records*
            when (<= start code end) return width)
      (error "No font metric for glyph ~X" code)))

(defparameter *player-name-max-glyphs* 4)
;; 0809670C formats a u16 using five decimal places; reserve the full range.
(defparameter *dynamic-number-max-glyphs* 5)

(defparameter *textbox-size* 208)

;; note: something special about text 1403 --- might be ducktor cid's crashing line

(defun scan-rom ()
  "Extract all known formats and audit every byte of the text section.
Optional detailed reports: tools/extract_text.py --report-dir dist/extraction."
  (uiop:run-program '("python" "tools/extract_text.py") :output *standard-output*
                    :error-output *error-output*))

(defun dump-all-text-utf (fname txts)
  (progn
    (with-open-file (stream fname :direction :output
                                  :external-format :utf-8
                                  :if-exists :supersede)
      (with-standard-io-syntax
        (let ((*print-readably* nil) (*print-pretty* nil)
              (*package* (find-package :slurp)))
          (loop for txt in txts do
            (prin1 txt stream)
            (terpri stream)))))
    nil))

(defun read-all-text-utf (fname)
  (with-open-file (stream fname :direction :input
                                :external-format :utf-8)
    (let ((*read-eval* nil) (*package* (find-package :slurp)))
      (loop for sexp = (read stream nil)
            while sexp
            collect sexp))))

(defun read-dialogue-text (path)
  (remove-if-not (lambda (row)
                   (and (<= 52 (car row) 2397) (not (<= 1883 (car row) 1901))))
                 (read-all-text-utf path)))

(defun trans (all-text-translated f)
  (let ((slime-patched (read-rom "slime_original.gba")))
    (load-font-metrics slime-patched)
    (validate-dialogue-entries slime-patched all-text-translated)
    (patch-text slime-patched
                (invert-alist (reverse (load-translation-table "SlimeDialog.tbl")))
                (invert-alist (reverse (load-translation-table "Slime_Small.tbl")))
                (mapcar #'reflow-string all-text-translated))
    (dump-rom slime-patched f)))

(define-condition malformed-string-error (error)
  ((text :initarg :text :reader text)))

(defun skip-malformed-string (c)
  (declare (ignore c))
  (invoke-restart 'skip-malformed-string))

(defun load-all-texts (rom pointer-table translation-table small-translation-table &optional (ignore nil))
  "Legacy indexed dialogue extraction. Invalid/null slots and credits are excluded.
Use SCAN-ROM for the complete, format-aware inventory including non-table text."
  (loop for (idx . addr) in pointer-table
        when (and (<= 52 idx 2397) (not (<= 1883 idx 1901))
                  (not (member idx ignore)) (<= #x08000000 addr)
                  (< (- addr #x08000000) (length rom)))
        collect (cons idx (decode-string translation-table small-translation-table
                                         (- addr #x08000000) rom))))

(defun invert-alist (alist)
  (loop for (a . b) in (stable-sort (copy-list alist) #'< :key #'car)
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
  (with-open-file (stream table-file :external-format :utf-8)
    (loop for line = (read-line stream nil)
          while line
          collect (parse-translation-table-entry line))))

(defun text-glyphs (string table)
  "Tokenize table entries atomically; never silently replace an unknown glyph."
  (loop with pos = 0
        while (< pos (length string))
        for match = (loop for entry in table
                          for key = (car entry)
                          when (and (<= (+ pos (length key)) (length string))
                                    (string= key string :start2 pos
                                                       :end2 (+ pos (length key))))
                            maximize (length key) into longest
                          finally
                             (return (find-if
                                      (lambda (entry)
                                        (and (= (length (car entry)) longest)
                                             (> longest 0)
                                             (<= (+ pos longest) (length string))
                                             (string= (car entry) string :start2 pos
                                                                      :end2 (+ pos longest))))
                                      table)))
        do (unless match
             (error "Unencodable text at position ~D: ~S" pos string))
        collect match
        do (incf pos (length (car match)))))

(defun reflow-string (entry &key (width *textbox-size*) (lines-per-page 2)
                                (paginate (some (lambda (x)
                                                  (or (and (consp x) (eq (car x) 'name))
                                                      (member x '((show-prompt) (wait-input) (yes-no)
                                                                  (wait-for-a)) :test #'equal)))
                                                (cdr entry))))
  "Reflow an indexed dialogue entry using ROM widths, preserving control order.
NEWLINE is soft unless it follows a wait; FORCE-NEWLINE is always hard.
Existing waits and terminal behavior are retained. No final wait is invented."
  (unless (and (integerp (car entry)) (plusp width) (plusp lines-per-page))
    (error "Expected an indexed text entry and a positive width: ~S" entry))
  (unless *font-records* (load-font-metrics))
  (let ((table (invert-alist (reverse (load-translation-table "SlimeDialog.tbl"))))
        (tokens nil) (out nil) (word nil) (pending-space nil)
        (pixels 0) (lines 1) (after-wait nil))
    (labels ((emit (x)
               (if (and (stringp x) (stringp (car out)))
                   (setf (car out) (concatenate 'string (car out) x))
                   (push x out)))
             (advance (items start)
               (loop with x = start
                     for item in items
                     do (cond
                          ((stringp item)
                           (dolist (g (text-glyphs item table))
                             (incf x (+ (glyph-width (cdr g)) (if (zerop x) 0 1)))))
                          ((and (consp item) (eq (car item) 'glyph))
                           (incf x (+ (glyph-width (cadr item)) (if (zerop x) 0 1))))
                          ((equal item '(player-name))
                           (incf x (- (* *player-name-max-glyphs*
                                        (1+ (reduce #'max *font-records* :key #'third)))
                                      (if (zerop x) 1 0))))
                          ((and (consp item) (eq (car item) 'dynamic-text))
                           (incf x (- (* *dynamic-number-max-glyphs* (1+ (glyph-width #x28)))
                                      (if (zerop x) 1 0)))))
                     finally (return x)))
             (line-break ()
               (when (and (>= lines lines-per-page) (not after-wait))
                 (unless paginate
                   (error "Entry ~D needs more than ~D lines; select a layout explicitly."
                          (car entry) lines-per-page))
                 (emit '(show-prompt)) (emit '(wait-input)) (setf lines 0))
               (emit '(newline))
               (setf pixels 0 pending-space nil after-wait nil)
               (incf lines))
             (flush-word ()
               (when word
                 (let* ((items (reverse word))
                        (space (and pending-space (> pixels 0)))
                        (end (advance items (if space (advance '(" ") pixels) pixels))))
                   (when (> end width)
                     (when (zerop pixels)
                       (error "Entry ~D has a word wider than ~D pixels: ~S" (car entry) width items))
                     (line-break)
                     (setf space nil end (advance items 0))
                     (when (> end width)
                       (error "Entry ~D has a word wider than ~D pixels: ~S" (car entry) width items)))
                   (when space (emit " "))
                   (dolist (item items) (emit item))
                   (setf pixels end word nil pending-space nil)))))
      ;; Keep color changes inside words, so punctuation and fragments around
      ;; zero-width controls cannot acquire or lose spaces.
      (dolist (item (cdr entry))
        (cond
          ((stringp item)
           (dolist (glyph (text-glyphs item table))
             (push (car glyph) tokens)))
          (t (push item tokens))))
      (dolist (item (nreverse tokens))
        (cond
          ((equal item " ") (flush-word) (setf pending-space t))
          ((equal item '(newline))
           (flush-word)
           (if after-wait (line-break) (setf pending-space t)))
          ((equal item '(force-newline)) (flush-word) (line-break))
          ((or (stringp item) (equal item '(player-name))
               (and (consp item) (member (car item) '(color glyph)))
               (and (consp item) (eq (car item) 'dynamic-text)))
           (push item word))
          (t
           (flush-word) (emit item)
           (cond
             ((member item '((wait-input) (wait-for-a)) :test #'equal)
              (setf after-wait t lines 0))
             ((member item '((scroll) (clear) (switch-window)) :test #'equal)
              (setf pixels 0 lines 1 pending-space nil after-wait nil))))))
      (flush-word)
      (cons (car entry) (nreverse out)))))

(defun prepare-dialogue-preview (entries)
  "Shared reflow/encoding pass for local ROM and ROM-free IPS builds.
Callers validate authoring syntax and supply *FONT-RECORDS* first.
Returns injectable entries and layout/encoding holds as two values."
  (unless *font-records* (error "Load font metrics before preparing a preview"))
  (let ((encoding (invert-alist (reverse (load-translation-table "SlimeDialog.tbl"))))
        (small (invert-alist (reverse (load-translation-table "Slime_Small.tbl"))))
        (passed nil) (failed nil))
    (dolist (entry entries)
      (handler-case
          (let ((flowed (reflow-string entry)))
            (encode-string encoding small (cdr flowed))
            (push flowed passed))
        (error (e) (push (list (car entry) (princ-to-string e)) failed))))
    (values (nreverse passed) (nreverse failed))))

(defun decode-string (translation-table small-translation-table offset rom)
  "Decode the dialogue grammar, consuming arguments even when they equal zero."
  (let ((pos offset) (out nil) (limit (min (length rom) (+ offset 16384))))
    (labels ((get-code ()
               (when (>= pos limit) (error "Unterminated dialogue at ~X" offset))
               (prog1 (elt rom pos) (incf pos)))
             (put (text)
               (if (stringp (car out))
                   (setf (car out) (concatenate 'string (car out) text))
                   (push text out)))
             (put-glyph (code)
               (let ((mapped (assoc code translation-table)))
                 (unless mapped (error "Unknown glyph ~X at ~X" code (1- pos)))
                 ;; Duplicate spellings are distinct, known font glyphs.
                 (if (member code '(#xf4 #x15e))
                     (push (list 'glyph code) out)
                     (put (cdr mapped))))))
      (loop for code = (get-code) do
        (cond
          ((zerop code) (return (nreverse out)))
          ((= code 1) (put-glyph (+ 256 (get-code))))
          ((>= code 16) (put-glyph code))
          ((= code 5)
           (push (list 'name
                       (with-output-to-string (name)
                         (loop for c = (get-code) until (= c 5) do
                           (let ((mapped (assoc c small-translation-table)))
                             (unless mapped (error "Unknown name glyph ~X" c))
                             (write-string (cdr mapped) name))))) out))
          (t
           (destructuring-bind (name opcode nargs)
               (find code *text-opcodes* :key #'second)
             (declare (ignore opcode))
             (push (cons name (loop repeat nargs collect (get-code))) out))))))))

(defun encode-string (inv-translation-table inv-translation-table-small tokens)
  "Encode named dialogue commands. Legacy BYTE/CONTROL forms must be migrated."
  (loop for token in tokens append
    (cond
      ((stringp token) (mapcar #'cdr (text-glyphs token inv-translation-table)))
      ((equal token '(wait-for-a)) '(7 8)) ; historical explicit shorthand
      ((equal token '(force-newline)) '(2))
      ((and (consp token) (eq (car token) 'name) (= (length token) 2)
            (stringp (cadr token)) (plusp (length (cadr token))))
       (append '(5) (mapcar #'cdr (text-glyphs (cadr token) inv-translation-table-small)) '(5)))
      ((and (consp token) (eq (car token) 'glyph) (= (length token) 2)
            (integerp (cadr token)) (<= #x10 (cadr token) #x1b8))
       (list (cadr token)))
      (t
       (let ((op (and (consp token) (assoc (car token) *text-opcodes*))))
         (unless (and op (= (length token) (1+ (third op)))
                      (every (lambda (arg) (typep arg '(unsigned-byte 8))) (cdr token)))
           (error "Unknown or malformed text command: ~S; migrate historical scripts with agent-tools/audit_text.py" token))
         (cons (second op) (cdr token)))))))

(defun verify-isomorphic (string tt tts)
  (let* ((encoded (encode-string (invert-alist tt) (invert-alist tts) string))
         (bytes (loop for code in encoded append
                       (if (> code 255) (list (ash code -8) (logand code 255)) (list code))))
         (decoded (decode-string tt tts 0 (append bytes '(0)))))
    (if (not (equal decoded string))
        string)))

(defun parse-pointer-table (table-data)
  (flet ((nums->pointer (b1 b2 b3 b4)
           (logior (ash b4 24) (ash b3 16) (ash b2 8) b1)))
    (loop for i from 0 below (length table-data) by 4
          for j = 0 then (1+ j)
          collect (cons j (nums->pointer (elt table-data (+ i 0))
                                         (elt table-data (+ i 1))
                                         (elt table-data (+ i 2))
                                         (elt table-data (+ i 3)))))))

(defun patch-text (rom encoding-table small-encoding-table new-strings)
  (flet ((insert-string (bytes)
           (loop for byte in bytes do
             (cond
               ((> byte #xFF)
                (vector-push-extend (ldb (byte 8 8) byte) rom)
                (vector-push-extend (ldb (byte 8 0) byte) rom))
               (t (vector-push-extend byte rom))))
           (vector-push-extend 0 rom))) ; null terminator
    (let ((pointer-patches nil))
      (loop for (table-index . string) in new-strings do
        (unless (and (integerp table-index) (<= 52 table-index 2397)
                     (not (<= 1883 table-index 1901)))
          (error "Not a dialogue slot: ~S" table-index))
        (when (assoc table-index pointer-patches)
          (error "Duplicate text index: ~D" table-index))
        (rom-text-offset rom table-index)
        (push (cons table-index (fill-pointer rom)) pointer-patches)
        (insert-string (encode-string encoding-table small-encoding-table string)))
      (when (> (fill-pointer rom) #x2000000)
        (error "Patched ROM exceeds the GBA cartridge address window"))
      (loop for (table-index . pointer) in pointer-patches do
        (let ((tbl-pos (+ (car pointer-table-pos) (* 4 table-index)))
              (table-entry (logior #x8000000 pointer)))
          (loop for i from 0 to 24 by 8
                for bt = (ldb (byte 8 i) table-entry)
                for pos from tbl-pos do
                  (setf (aref rom pos) bt)))))))

(defun rom-text-offset (rom index)
  (unless (and (integerp index) (<= 52 index 2397))
    (error "Invalid dialogue index: ~S" index))
  (let* ((pos (+ (car pointer-table-pos) (* 4 index)))
         (pointer (loop for i below 4 sum (ash (aref rom (+ pos i)) (* 8 i))))
         (offset (- pointer #x08000000)))
    (unless (<= 0 offset (1- (length rom)))
      (error "Entry ~D has a non-ROM pointer: ~8,'0X" index pointer))
    offset))

(defun validate-dialogue-entries (rom entries)
  "Validate slots, syntax and wait boundaries without comparing original dialogue."
  (let ((table (invert-alist (reverse (load-translation-table "SlimeDialog.tbl"))))
        (small (invert-alist (reverse (load-translation-table "Slime_Small.tbl")))))
    (dolist (entry entries)
      (when (<= 1883 (car entry) 1901)
        (error "Entry ~D is credits data; dialogue reflow is inappropriate" (car entry)))
      (rom-text-offset rom (car entry))
      (encode-string table small (cdr entry))
      (loop for (token next) on (cdr entry)
            when (and next (member token '((wait-input) (wait-for-a)) :test #'equal)
                      (not (member next '((newline) (force-newline) (clear)
                                          (scroll) (switch-window)) :test #'equal)))
            do (error "Entry ~D is missing a paragraph separator after input wait" (car entry))))))

(defun read-rom (rom &optional (expansion #x100000)) ;expand by 1MB
  (with-open-file (stream rom :element-type '(unsigned-byte 8))
    (let ((arr (make-array (+ (file-length stream) expansion)
                           :element-type '(unsigned-byte 8)
                           :adjustable t
                           :fill-pointer (file-length stream))))
      (read-sequence arr stream)
      arr)))

(defun dump-rom (rom file)
  (with-open-file (stream file
                          :element-type '(unsigned-byte 8)
                          :direction :output
                          :if-exists :supersede)
    (write-sequence rom stream)))

(defun main ()
  (format t "loading texts...~%")
  (load-texts)
  (format t "injecting...~%")
  (trans txt "test.gba")
  (format t "created test.gba~%")
  (uiop:quit))

(defun build-exe ()
  (sb-ext:save-lisp-and-die "injector" :toplevel #'main :executable t :compression 9))
