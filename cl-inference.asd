;;;; cl-inference.asd

(asdf:defsystem #:cl-inference
  :description "Describe cl-inference here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on ("alexandria")
  :components ((:module "src"
			:serial t
			:components ((:file "package")
				     (:file "cl-inference")))))



