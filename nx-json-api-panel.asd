;;;; nx-json-api-panel.asd

(asdf:defsystem #:nx-json-api-panel
  :description "Nyxt extension to quickly query JSON APIs"
  :author "dpattila"
  :license  "BSD 3-clause"
  :version "0.0.1"
  :serial t
  :depends-on (#:nyxt)
  :components ((:file "package")
               (:file "nx-json-api-panel")))
