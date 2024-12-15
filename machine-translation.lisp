;; This file is just a dumping ground for machine translation stuff which is sort of no longer
;; used since there's an actual translation now.

(defun get-in (indices list)
  (loop for index in indices do
    (cond
      ((numberp index) (setf list (elt list index)))
      (t (setf list (cdr (assoc index list)))))
        finally (return list)))

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

;; This block of code isn't wrapped in a function so that if the process gets interrupted, we can still
;; salvage the already-processed translated texts, as running everything through the API takes a while.
(loop for string in *all-texts* do
  (let* ((stripped (remove-reflow-opcodes string))
         (translated (translate-string stripped)))
    (print translated)
    (push translated *all-text-translated*)))

;; All the necessary data to create *all-texts*, which is the useful bit of data we need
;; to create *all-text-translated*, which is injected back into the ROM.
(defparameter *rom-data* (read-rom "slime_original.gba"))
(defparameter *pointer-table-data* (apply #'subseq *rom-data* pointer-table-pos))
(defparameter *pointer-table* (parse-pointer-table *pointer-table-data*))
(defparameter *pointer-table2* (subseq *pointer-table* 53 2299))
(defparameter *translation-table* (reverse (load-translation-table "SlimeDialog.tbl")))
(defparameter *small-translation-table* (reverse (load-translation-table "Slime_Small.tbl")))
(defparameter *ignore* (append (loop for i from 0 to 51 collect i)
                               '(1892 1895 1897 1900)))
(defparameter *all-texts*
  (handler-bind ((malformed-string-error #'skip-malformed-string))
    (load-all-texts *rom-data* *pointer-table* *translation-table* *small-translation-table* *ignore*)))
