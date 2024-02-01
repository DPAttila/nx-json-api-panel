;;;; nx-json-api-panel.lisp

(in-package #:nx-json-api-panel)

(defun embed-query-text (alist query-text)
  "Evaluate/Replace format string values (cdr-s) in an ALIST,
  the format argument being QUERY-TEXT."
  (mapcar 
   (lambda (q) 
     (cons 
      (car q) 
      (if (stringp (cdr q))
          (format nil (cdr q) query-text)
          (cdr q)))) 
   alist))

(defun get-query-url (word host path-format-str query-params &key (scheme "https"))
  "Determine the API endpoint"
  (let* ((path (format nil path-format-str word))
         (query-url (quri:make-uri 
                     :scheme scheme
                     :host host
                     :path path
                     :query query-params)))
    (quri:render-uri query-url)))

(defun sanitize-query (query)
  "Convert to lowercase and strip the query of unexpected whitespaces"
  (funcall
   (alexandria:compose 
    #'string-downcase
    (lambda (q) (string-trim '(#\Space #\Tab #\Newline) q))
    (lambda (q) (cl-ppcre:regex-replace-all "\\s+" q " "))) 
   query))

(defun execute-request (url method &key (content nil))
  "Send the API request with dexador. In case of an error response,
  inform the user."
  (format t "~A%" url)
  (multiple-value-bind (body status res-headers) 
      (handler-case 
          (dex:request url :method method :content content)
        (dex:http-request-failed (e)
          (format t "HTTP Request Error: ~A%" e)
          (values 
           (dex:response-body e) 
           (dex:response-status e)
           (dex:response-headers e)))
        (error (e)
          (format t "General Error: ~A~%" e)
          (values (format nil "Error: ~A~%" e) 400 nil)))
    (format t "body: ~A ~% status: ~A ~% res-headers: ~A ~%" 
            body status (json-to-html res-headers))
    body))

(defun add-img-tag-if-needed (str s)
  "Add <img>-s to mark images in the string."
  (multiple-value-bind (str match) 
      (cl-ppcre:regex-replace-all 
       "(https:\/\/.*\.(?:jpg)|(?:png)|(?:gif)|(?:webp)|(?:svg)|(?:jpeg))"
       str
       "<br/><img style=\"max-width: 100%;\" src=\"\\1\">")
    (when match (format s str))
    match))

(defun add-anchor-tag-if-needed (str s)
  "Add <a>-s to mark links in the string."
  (multiple-value-bind (str match)
      (cl-ppcre:regex-replace-all
       "((?:(?:http(?:s){0,1}:\/\/)|(?:www\.))\\w\.\\S*)"
       str
       "<a target=\"_blank\" href=\"\\1\">\\1</a>")
    (when match (format s str))
    match))

(defun render-response (body)
  "Render the response as JSON by default. If can't convert to JSON, 
   display the content informing the user of the format error."
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
  "Stylize a JSON object to be presented in the panel as HTML"
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
     &key (method :GET)
          (scheme "https")
          (query-params nil) 
          (content nil)
          (panel-title-format-str (format nil "~A: ~~A" (symbol-name command-name))))
  "Add a custom command to query an API based on the content of the user's clipboard (selection).

  API-PATH-FORMAT-STR is a control string in which the selection may be embedded.

  QUERY-PARAMS is an alist that gets converted to a query string in the URL.
  String values (not keys) are interpreted as control strings, the format argument being the user's selection.

  CONTENT is an alist that is converted to a json and sent as the body of the request.
  String values (not keys) are interpreted as control strings, the format argument being the user's selection.

  PANTEL-TITLE-FORMAT-STR is a control string to display a title for the panel.
  "

  (nyxt:define-panel-command-global 
      ,command-name
    nil
    (nyxt:panel-buffer ,(symbol-name command-name))

    ;; Copy selection to clipboard 
    ;; (Maybe this text selection should be handled internally, without 
    ;; the use of the system clipboard?)
    (nyxt:ffi-buffer-copy (nyxt:current-buffer))

    (let* ((selection 
             (metabang.cl-containers:delete-first (nyxt:clipboard-ring nyxt:*browser*)))
           (selection-sanitized 
             (sanitize-query selection))
           (content-prepared 
             (njson:encode
              (embed-query-text ,content selection-sanitized)))
           (url
             (get-query-url 
              selection-sanitized
              ,api-host 
              ,api-path-format-str 
              (embed-query-text ,query-params selection-sanitized)
              :scheme ,scheme))
           (response (execute-request url ,method :content content-prepared)))
      (nyxt:echo url)

      (format nil "<h4>~A</h4> ~A" 
              (format nil ,panel-title-format-str selection-sanitized)
              (render-response response)))))


;; Default API added to verify the existence and functioning of the extension
(nx-json-api-panel:add-api 
 dictionary-api 
 "api.dictionaryapi.dev" 
 "/api/v2/entries/en/~A")
