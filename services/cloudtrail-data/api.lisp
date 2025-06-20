;; DO NOT EDIT: File is generated by AWS-SDK/GENERATOR.

(uiop/package:define-package #:aws-sdk/services/cloudtrail-data/api (:use)
                             (:nicknames #:aws/cloudtrail-data)
                             (:import-from #:aws-sdk/generator/shape)
                             (:import-from #:aws-sdk/generator/operation)
                             (:import-from #:aws-sdk/api)
                             (:import-from #:aws-sdk/request)
                             (:import-from #:aws-sdk/json-request)
                             (:import-from #:aws-sdk/rest-json-request)
                             (:import-from #:aws-sdk/rest-xml-request)
                             (:import-from #:aws-sdk/query-request)
                             (:import-from #:aws-sdk/error))
(common-lisp:in-package #:aws-sdk/services/cloudtrail-data/api)
(common-lisp:progn
 (common-lisp:define-condition cloudtrail-data-error
     (aws-sdk/error:aws-error)
     common-lisp:nil)
 (common-lisp:export 'cloudtrail-data-error))
(common-lisp:progn
 (common-lisp:defclass cloudtrail-data-request
                       (aws-sdk/generator/service::rest-json-request)
                       common-lisp:nil
                       (:default-initargs :service "cloudtrail-data"
                        :api-version "2021-08-11" :host-prefix
                        "cloudtrail-data" :signing-name "cloudtrail-data"
                        :global-host common-lisp:nil))
 (common-lisp:export 'cloudtrail-data-request))
(common-lisp:defvar *error-map*
  '(("ChannelInsufficientPermission" . channel-insufficient-permission)
    ("ChannelNotFound" . channel-not-found)
    ("ChannelUnsupportedSchema" . channel-unsupported-schema)
    ("DuplicatedAuditEventId" . duplicated-audit-event-id)
    ("InvalidChannelARN" . invalid-channel-arn)
    ("UnsupportedOperationException" . unsupported-operation-exception)))
(common-lisp:progn
 (common-lisp:defclass audit-event common-lisp:nil
                       ((event-data :initarg :event-data :initform
                         (common-lisp:error ":eventdata is required") :type
                         (common-lisp:or string common-lisp:null) :accessor
                         struct-shape-audit-event-event-data :shape "String"
                         :location common-lisp:nil :location-name
                         common-lisp:nil)
                        (event-data-checksum :initarg :event-data-checksum
                         :initform common-lisp:nil :type
                         (common-lisp:or string common-lisp:null) :accessor
                         struct-shape-audit-event-event-data-checksum :shape
                         "String" :location common-lisp:nil :location-name
                         common-lisp:nil)
                        (id :initarg :id :initform
                         (common-lisp:error ":id is required") :type
                         (common-lisp:or uuid common-lisp:null) :accessor
                         struct-shape-audit-event-id :shape "Uuid" :location
                         common-lisp:nil :location-name common-lisp:nil))
                       (:metaclass aws-sdk/generator/shape::members-class))
 (common-lisp:defun make-audit-event
                    (common-lisp:&rest aws-sdk/generator/shape::args)
   (common-lisp:apply #'common-lisp:make-instance 'audit-event
                      aws-sdk/generator/shape::args))
 (common-lisp:export (common-lisp:list 'audit-event 'make-audit-event))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input audit-event))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input audit-event))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'event-data))
      (common-lisp:list
       (common-lisp:cons "eventData"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'event-data-checksum))
      (common-lisp:list
       (common-lisp:cons "eventDataChecksum"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'id))
      (common-lisp:list
       (common-lisp:cons "id"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input audit-event))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:deftype audit-event-result-entries ()
   '(trivial-types:proper-list audit-event-result-entry))
 (common-lisp:defun make-audit-event-result-entries
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list
                            audit-event-result-entry))
   aws-sdk/generator/shape::members))
(common-lisp:progn
 (common-lisp:defclass audit-event-result-entry common-lisp:nil
                       ((event-id :initarg :event-id :initform
                         (common-lisp:error ":eventid is required") :type
                         (common-lisp:or uuid common-lisp:null) :accessor
                         struct-shape-audit-event-result-entry-event-id :shape
                         "Uuid" :location common-lisp:nil :location-name
                         common-lisp:nil)
                        (id :initarg :id :initform
                         (common-lisp:error ":id is required") :type
                         (common-lisp:or uuid common-lisp:null) :accessor
                         struct-shape-audit-event-result-entry-id :shape "Uuid"
                         :location common-lisp:nil :location-name
                         common-lisp:nil))
                       (:metaclass aws-sdk/generator/shape::members-class))
 (common-lisp:defun make-audit-event-result-entry
                    (common-lisp:&rest aws-sdk/generator/shape::args)
   (common-lisp:apply #'common-lisp:make-instance 'audit-event-result-entry
                      aws-sdk/generator/shape::args))
 (common-lisp:export
  (common-lisp:list 'audit-event-result-entry 'make-audit-event-result-entry))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          audit-event-result-entry))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          audit-event-result-entry))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'event-id))
      (common-lisp:list
       (common-lisp:cons "eventID"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'id))
      (common-lisp:list
       (common-lisp:cons "id"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          audit-event-result-entry))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:deftype audit-events () '(trivial-types:proper-list audit-event))
 (common-lisp:defun make-audit-events
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list audit-event))
   aws-sdk/generator/shape::members))
(common-lisp:deftype channel-arn () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:define-condition channel-insufficient-permission
     (cloudtrail-data-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       channel-insufficient-permission-message)))
 (common-lisp:export
  (common-lisp:list 'channel-insufficient-permission
                    'channel-insufficient-permission-message)))
(common-lisp:progn
 (common-lisp:define-condition channel-not-found
     (cloudtrail-data-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       channel-not-found-message)))
 (common-lisp:export
  (common-lisp:list 'channel-not-found 'channel-not-found-message)))
(common-lisp:progn
 (common-lisp:define-condition channel-unsupported-schema
     (cloudtrail-data-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       channel-unsupported-schema-message)))
 (common-lisp:export
  (common-lisp:list 'channel-unsupported-schema
                    'channel-unsupported-schema-message)))
(common-lisp:progn
 (common-lisp:define-condition duplicated-audit-event-id
     (cloudtrail-data-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       duplicated-audit-event-id-message)))
 (common-lisp:export
  (common-lisp:list 'duplicated-audit-event-id
                    'duplicated-audit-event-id-message)))
(common-lisp:deftype error-code () 'common-lisp:string)
(common-lisp:deftype error-message () 'common-lisp:string)
(common-lisp:deftype external-id () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:define-condition invalid-channel-arn
     (cloudtrail-data-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       invalid-channel-arn-message)))
 (common-lisp:export
  (common-lisp:list 'invalid-channel-arn 'invalid-channel-arn-message)))
(common-lisp:progn
 (common-lisp:defclass put-audit-events-request common-lisp:nil
                       ((audit-events :initarg :audit-events :initform
                         (common-lisp:error ":auditevents is required") :type
                         (common-lisp:or audit-events common-lisp:null)
                         :accessor
                         struct-shape-put-audit-events-request-audit-events
                         :shape "AuditEvents" :location common-lisp:nil
                         :location-name common-lisp:nil)
                        (channel-arn :initarg :channel-arn :initform
                         (common-lisp:error ":channelarn is required") :type
                         (common-lisp:or channel-arn common-lisp:null)
                         :accessor
                         struct-shape-put-audit-events-request-channel-arn
                         :shape "ChannelArn" :location "querystring"
                         :location-name "channelArn")
                        (external-id :initarg :external-id :initform
                         common-lisp:nil :type
                         (common-lisp:or external-id common-lisp:null)
                         :accessor
                         struct-shape-put-audit-events-request-external-id
                         :shape "ExternalId" :location "querystring"
                         :location-name "externalId"))
                       (:metaclass aws-sdk/generator/shape::members-class))
 (common-lisp:defun make-put-audit-events-request
                    (common-lisp:&rest aws-sdk/generator/shape::args)
   (common-lisp:apply #'common-lisp:make-instance 'put-audit-events-request
                      aws-sdk/generator/shape::args))
 (common-lisp:export
  (common-lisp:list 'put-audit-events-request 'make-put-audit-events-request))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          put-audit-events-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          put-audit-events-request))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'audit-events))
      (common-lisp:list
       (common-lisp:cons "auditEvents"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          put-audit-events-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defclass put-audit-events-response common-lisp:nil
                       ((failed :initarg :failed :initform
                         (common-lisp:error ":failed is required") :type
                         (common-lisp:or result-error-entries common-lisp:null)
                         :accessor
                         struct-shape-put-audit-events-response-failed :shape
                         "ResultErrorEntries" :location common-lisp:nil
                         :location-name common-lisp:nil)
                        (successful :initarg :successful :initform
                         (common-lisp:error ":successful is required") :type
                         (common-lisp:or audit-event-result-entries
                                         common-lisp:null)
                         :accessor
                         struct-shape-put-audit-events-response-successful
                         :shape "AuditEventResultEntries" :location
                         common-lisp:nil :location-name common-lisp:nil))
                       (:metaclass aws-sdk/generator/shape::members-class))
 (common-lisp:defun make-put-audit-events-response
                    (common-lisp:&rest aws-sdk/generator/shape::args)
   (common-lisp:apply #'common-lisp:make-instance 'put-audit-events-response
                      aws-sdk/generator/shape::args))
 (common-lisp:export
  (common-lisp:list 'put-audit-events-response
                    'make-put-audit-events-response))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          put-audit-events-response))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          put-audit-events-response))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'failed))
      (common-lisp:list
       (common-lisp:cons "failed"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'successful))
      (common-lisp:list
       (common-lisp:cons "successful"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          put-audit-events-response))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:deftype result-error-entries ()
   '(trivial-types:proper-list result-error-entry))
 (common-lisp:defun make-result-error-entries
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list result-error-entry))
   aws-sdk/generator/shape::members))
(common-lisp:progn
 (common-lisp:defclass result-error-entry common-lisp:nil
                       ((error-code :initarg :error-code :initform
                         (common-lisp:error ":errorcode is required") :type
                         (common-lisp:or error-code common-lisp:null) :accessor
                         struct-shape-result-error-entry-error-code :shape
                         "ErrorCode" :location common-lisp:nil :location-name
                         common-lisp:nil)
                        (error-message :initarg :error-message :initform
                         (common-lisp:error ":errormessage is required") :type
                         (common-lisp:or error-message common-lisp:null)
                         :accessor
                         struct-shape-result-error-entry-error-message :shape
                         "ErrorMessage" :location common-lisp:nil
                         :location-name common-lisp:nil)
                        (id :initarg :id :initform
                         (common-lisp:error ":id is required") :type
                         (common-lisp:or uuid common-lisp:null) :accessor
                         struct-shape-result-error-entry-id :shape "Uuid"
                         :location common-lisp:nil :location-name
                         common-lisp:nil))
                       (:metaclass aws-sdk/generator/shape::members-class))
 (common-lisp:defun make-result-error-entry
                    (common-lisp:&rest aws-sdk/generator/shape::args)
   (common-lisp:apply #'common-lisp:make-instance 'result-error-entry
                      aws-sdk/generator/shape::args))
 (common-lisp:export
  (common-lisp:list 'result-error-entry 'make-result-error-entry))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input result-error-entry))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input result-error-entry))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'error-code))
      (common-lisp:list
       (common-lisp:cons "errorCode"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'error-message))
      (common-lisp:list
       (common-lisp:cons "errorMessage"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'id))
      (common-lisp:list
       (common-lisp:cons "id"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input result-error-entry))
   common-lisp:nil))
(common-lisp:deftype string () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:define-condition unsupported-operation-exception
     (cloudtrail-data-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       unsupported-operation-exception-message)))
 (common-lisp:export
  (common-lisp:list 'unsupported-operation-exception
                    'unsupported-operation-exception-message)))
(common-lisp:deftype uuid () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defun put-audit-events
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key audit-events channel-arn external-id)
   (common-lisp:declare
    (common-lisp:ignorable audit-events channel-arn external-id))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply 'make-put-audit-events-request
                                         aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input
        'cloudtrail-data-request aws-sdk/generator/operation::input "POST"
        "/PutAuditEvents" "PutAuditEvents"))
      common-lisp:nil common-lisp:nil *error-map* "application/json")))
 (common-lisp:export 'put-audit-events))
