;;;; nx-json-api-panel.lisp

(in-package #:nx-json-api-panel)


(defun get-query-url (word host path-format-str query-param-format-strs)
  (let* ((path (format nil path-format-str word))
         (query-params 
           (mapcar 
             (lambda (q) (cons (car q) (format nil (cdr q) word))) 
             query-param-format-strs))
         (query-url (quri:make-uri 
               :scheme "https" 
               :host host
               :path path
               :query query-params
               )))
    (quri:render-uri query-url)))

(defun sanitize-query (query)
  (funcall
    (alexandria:compose 
      #'string-downcase
      (lambda (q) (string-trim '(#\Space #\Tab #\Newline) q))
      (lambda (q) (cl-ppcre:regex-replace-all "\\s+" q " "))) 
    query))

(defun execute-request (url)
  (multiple-value-bind (body status res-headers) 
    (handler-case 
      (dex:get url)
      (dex:http-request-failed (e)
        (format t "HTTP Request Error: ~A" e)
        (values 
          (dex:response-body e) 
          (dex:response-status e)
          (dex:response-headers e))))
    (format t "body: ~A ~% status: ~A ~% res-headers: ~A ~%" 
             body status (json-to-html res-headers))
    body))

(defun add-img-tag-if-needed (str s)
  (multiple-value-bind (str match) 
    (cl-ppcre:regex-replace-all 
      "(https:\/\/.*\.(?:jpg)|(?:png)|(?:gif)|(?:webp)|(?:svg)|(?:jpeg))"
      str
      "<br/><img style=\"max-width: 100%;\" src=\"\\1\">")
    (when match (format s str))
    match))

(defun add-anchor-tag-if-needed (str s)
  (multiple-value-bind (str match)
    (cl-ppcre:regex-replace-all
      "((?:(?:http(?:s){0,1}:\/\/)|(?:www\.))\\w\.\\S*)"
      str
      "<a target=\"_blank\" href=\"\\1\">\\1</a>")
    (when match (format s str))
    match))

(defun render-response (body)
  (handler-case
    (json-to-html 
      (njson:decode body))
    (njson:jerror (e)
      (format t "Error while decoding json: ~A~%" e)
      (format nil "Could not decode response as json: ~A" body))
    (error (e)
      (format t "Error while decoding json: ~A~%" e)
      (format nil "Could not decode response as json: ~A" body))))

(defun json-to-html (json-obj)
  (with-output-to-string (s)
    (format s "<div style=\"font-size:small;\">")
    (pretty-print-to-stream json-obj s)
    (format s "</div>")))

(defun pretty-print-to-stream (obj s &key (level 0))
  (format s "~V@T" level)

  (let ((li-open "<li style=\"text-indent: 0px;\">")
        (ul-open "<ul style=\"padding-left: 5px; text-indent: 0px;\">"))
    (cond
      ((stringp obj) 
         (when (not (or  
                  (add-img-tag-if-needed obj s)
                  (add-anchor-tag-if-needed obj s)))
         (format s "~A~%" obj)))
      ((vectorp obj) 
       (format s ul-open)
        (mapc 
          (lambda (x) 
            (format s li-open)
            (pretty-print-to-stream x s :level (+ level 1))
            (format s "</li>")) 
          (coerce obj 'list))
        (format s "</ul>"))
      ((hash-table-p obj)
        (maphash (lambda (k v)
                 (format s "<strong>~A</strong>:" k) 
                 (pretty-print-to-stream v s :level (+ level 1))
                 (format s "<br/>"))
               obj))
      (t (format s "~A~%" obj)))))

(defmacro add-api 
  (command-name 
   api-host 
   api-path-format-str 
   &key (query-params '()) 
   (panel-title-format-str (format nil "~A: ~~A" (symbol-name command-name))))

  `(nyxt:define-panel-command-global 
    ,command-name
    nil
    (nyxt:panel-buffer ,(symbol-name command-name))

    ;; Copy selection to clipboard 
    ;; (Maybe this text selection should be handled internally, without 
    ;; the use of the system clipboard?)
    (nyxt:ffi-buffer-copy (nyxt:current-buffer))

    (let* ((selection (metabang.cl-containers:delete-first 
                       (nyxt:clipboard-ring nyxt:*browser*)))
           (selection-sanitized (sanitize-query selection))
           (url
            (get-query-url 
              selection-sanitized
              ,api-host 
              ,api-path-format-str 
              ,query-params))
           (response
            (execute-request url)))
        (nyxt:echo url)

        (format t (render-response response))

        (format nil "<h4>~A</h4> ~A" 
                (format nil ,panel-title-format-str selection-sanitized)
                (render-response response)))))

(nx-json-api-panel:add-api dictionary-api "api.dictionaryapi.dev" "/api/v2/entries/en/~A")
(nx-json-api-panel:add-api country-api "restcountries.com" "/v3.1/name/~A")
(nx-json-api-panel:add-api 
  weather-api 
  "api.weatherapi.com" 
  "/v1/current.json" 
  :query-params '(("key" . "2a025a6562694db5bc0144313232912") 
                 ("aqi" . "no")
                 ("q" . "~A"))
  :panel-title-format-str "Weather in ~A")

(nx-json-api-panel:add-api 
  wikipedia-api 
  "en.wikipedia.org"
  "/api/rest_v1/page/summary/~A")

(nx-json-api-panel:add-api
  bad-api
  "bad"
  "api")
