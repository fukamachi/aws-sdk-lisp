(defpackage #:aws-sdk/generator/service
  (:use #:cl
        #:aws-sdk/generator/operation
        #:aws-sdk/generator/shape
        #:aws-sdk/error
        #:aws-sdk/utils)
  (:import-from #:aws-sdk/request
                #:request)
  (:import-from #:aws-sdk/query-request
                #:query-request)
  (:import-from #:aws-sdk/json-request
                #:json-request)
  (:import-from #:aws-sdk/rest-xml-request
                #:rest-xml-request)
  (:import-from #:yason)
  (:import-from #:alexandria
                #:when-let*
                #:when-let
                #:make-keyword)
  (:export #:dump-service
           #:load-service
           #:generate-service
           #:generate-all-services))
(in-package #:aws-sdk/generator/service)

(defpackage #:aws-sdk/generator/service/dump)

(defun dump-service-api-to-stream (service json &optional (stream *standard-output*))
  (let ((*package* (find-package :aws-sdk/generator/service/dump))
        (*print-case* :downcase)
        (package-name (make-symbol (format nil "~:@(~A/~A/~A/~A~)" :aws-sdk :services service :api))))
    (let ((exception-name (intern (format nil "~:@(~A-ERROR~)" service)))
          (request-name (intern (format nil "~:@(~A-REQUEST~)" service))))
      (format stream "~&;; DO NOT EDIT: File is generated by AWS-SDK/GENERATOR.~2%")
      (format stream "~&~S~%"
              `(uiop:define-package ,package-name
                 (:use)
                 (:nicknames ,(make-symbol (format nil "~:@(~A/~A~)" :aws service)))
                 (:import-from #:aws-sdk/generator/shape)
                 (:import-from #:aws-sdk/generator/operation)
                 (:import-from #:aws-sdk/api)
                 (:import-from #:aws-sdk/request)
                 (:import-from #:aws-sdk/json-request)
                 (:import-from #:aws-sdk/rest-json-request)
                 (:import-from #:aws-sdk/rest-xml-request)
                 (:import-from #:aws-sdk/query-request)
                 (:import-from #:aws-sdk/error)))
      (format stream "~&~S~%" `(in-package ,package-name))
      (format stream "~&~S~%"
              `(progn
                 (define-condition ,exception-name (aws-error) ())
                 (export ',exception-name)))
      (let* ((hash (yason:parse
                     (uiop:read-file-string json)))
             (protocol (make-keyword (string-upcase (gethash+ '("metadata" "protocol") hash))))
             (error-map (loop for name being each hash-key of (gethash "shapes" hash)
                              using (hash-value options)
                              if (gethash "exception" options)
                              collect (cons name (lispify name)))))
        (format stream "~&~S~%"
                `(progn
                   (defclass ,request-name (,(case protocol
                                               (:json 'json-request)
                                               ((:query :ec2) 'query-request)
                                               (:rest-json 'rest-json-request)
                                               (:rest-xml 'rest-xml-request)))
                     ()
                     (:default-initargs :service ,service
                                        :api-version ,(gethash+ '("metadata" "apiVersion") hash)
                                        :host-prefix ,(gethash+ '("metadata" "endpointPrefix") hash)
                                        :signing-name ,(gethash+ '("metadata" "signingName") hash)
                                        :global-host ,(gethash+ '("metadata" "globalEndpoint") hash)
                      ,@(when (eq protocol :json)
                          (list :target-prefix (gethash+ '("metadata" "targetPrefix") hash)
                                :json-version (gethash+ '("metadata" "jsonVersion") hash)))))
                   (export ',request-name)))
        (format stream "~&~S~%"
                `(defvar ,(intern (string :*error-map*)) ',error-map))

        (loop for name being each hash-key of (gethash "shapes" hash)
              using (hash-value options)
              do (format stream "~&~S~%" (compile-shape name options exception-name)))

        (loop for action being each hash-key of (gethash "operations" hash)
              using (hash-value options)
              for input = (gethash "input" options)
              for output = (gethash "output" options)
              do (format stream "~&~S~%"
                         (compile-operation
                          service
                          action
                          options
                          (and input
                               (loop for key being each hash-key of (gethash+ `("shapes" ,(gethash "shape" input) "members")
                                                                              hash)
                                     collect (lispify key)))
                          (and output
                               (when-let* ((payload-shape
                                            (gethash+ `("shapes" ,(gethash "shape" output) "payload") hash))
                                           (payload-shape (gethash+ `("shapes"
                                                                      ,(gethash "shape" output)
                                                                      "members"
                                                                      ,payload-shape)
                                                                    hash)))
                                 (labels ((find-output-type (shape)
                                            (and shape
                                                 (or (gethash "type" shape)
                                                     (find-output-type
                                                      (gethash+ `("shapes" ,(gethash "shape" shape)) hash))))))
                                   (find-output-type payload-shape))))
                          (intern (string :*error-map*))
                          protocol)))
        (force-output stream)))))

(defun dump-service-base-file-to-stream (service service-dir &optional (stream *standard-output*))
  (let ((*package* (find-package :aws-sdk/generator/service/dump))
        (*print-case* :downcase)
        (package-name (make-symbol (format nil "~:@(~A/~A/~A~)" :aws-sdk :services service))))
    (format stream "~&;; DO NOT EDIT: File is generated by AWS-SDK/GENERATOR.~2%")
    (format stream "~&(uiop:define-package #:~A
  (:use)
  (:use-reexport ~{#:~A~^~%                 ~}))~%"
            package-name
            (mapcar (lambda (path)
                      (format nil "~A/~A" package-name (pathname-name path)))
                    (uiop:directory-files service-dir)))
    (force-output stream)))

(defun dump-service (service json output-dir)
  (let* ((service-directory (uiop:ensure-directory-pathname (merge-pathnames service output-dir)))
         (api-path (merge-pathnames "api.lisp" service-directory))
         (base-path (merge-pathnames (make-pathname :name service :type "lisp")
                                     output-dir))
         (*print-right-margin* 80))
    (ensure-directories-exist service-directory)
    (with-open-file (out api-path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (dump-service-api-to-stream service json out))
    (with-open-file (out base-path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (when out
        (dump-service-base-file-to-stream service service-directory out)))
    base-path))

(defun load-service (service json)
  (uiop:with-temporary-file (:pathname file :direction :output)
    (dump-service service json file)
    (compile-file file)
    (load file)))

(defun generate-service (service)
  (let* ((output-dir (asdf:system-relative-pathname :aws-sdk #P"services/"))
         (service-dir
           (asdf:system-relative-pathname :aws-sdk
                                          (make-pathname :directory `(:relative "specs" "apis" ,service))))
         (api-2.json
           (merge-pathnames #P"api-2.json"
                            (car (last (uiop:subdirectories service-dir))))))
    (assert (probe-file api-2.json))
    (dump-service service api-2.json output-dir)))

(defun generate-all-services (&key silent)
  (dolist (service-dir
           (uiop:subdirectories (asdf:system-relative-pathname :aws-sdk #P"specs/apis/")))
    (let ((file (generate-service (car (last (pathname-directory service-dir))))))
      (unless silent
        (format t "~&Generated '~A'~%" file)))))
