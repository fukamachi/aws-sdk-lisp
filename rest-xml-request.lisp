(defpackage #:aws-sdk/rest-xml-request
  (:use #:cl)
  (:import-from #:aws-sdk/request
                #:request
                #:request-headers
                #:request-path
                #:request-payload)
  (:import-from #:aws-sdk/session
                #:*session*)
  (:import-from #:xml-emitter)
  (:export #:rest-xml-request))
(in-package #:aws-sdk/rest-xml-request)

(defclass rest-xml-request (request)
  ())

(defun object-to-xml (object location-name &optional xml-namespace)
  (assert location-name)
  (typecase object
    (null)
    ;; XXX: Should see if it's flattened or not.
    (cons (map nil (lambda (obj) (object-to-xml obj location-name))
               object))
    (standard-object
     (xml-emitter:with-simple-tag (location-name nil xml-namespace)
       (loop for slot in (c2mop:class-direct-slots (class-of object))
             for value = (slot-value object (c2mop:slot-definition-name slot))
             for tag-name = (or (aws-sdk/generator/shape::member-slot-location-name slot)
                                (aws-sdk/generator/shape::member-slot-shape slot))
             when value
             do (if tag-name
                    (object-to-xml value tag-name)
                    (xml-emitter:xml-out value)))))
    (otherwise
     (xml-emitter:simple-tag location-name object nil xml-namespace))))

(defmethod initialize-instance :after ((req rest-xml-request) &rest args &key path payload payload-properties &allow-other-keys)
  (declare (ignore args))
  (when (typep payload 'standard-object)
    (destructuring-bind (&key location-name xml-namespace)
        payload-properties
      (setf (request-payload req)
            (with-output-to-string (s)
              (xml-emitter:with-xml-output (s :encoding "UTF-8")
                (object-to-xml payload location-name xml-namespace))))))
  (setf (request-path req) path))
