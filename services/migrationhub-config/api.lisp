;; DO NOT EDIT: File is generated by AWS-SDK/GENERATOR.

(uiop/package:define-package #:aws-sdk/services/migrationhub-config/api (:use)
                             (:nicknames #:aws/migrationhub-config)
                             (:import-from #:aws-sdk/generator/shape)
                             (:import-from #:aws-sdk/generator/operation)
                             (:import-from #:aws-sdk/api)
                             (:import-from #:aws-sdk/request)
                             (:import-from #:aws-sdk/json-request)
                             (:import-from #:aws-sdk/rest-json-request)
                             (:import-from #:aws-sdk/rest-xml-request)
                             (:import-from #:aws-sdk/query-request)
                             (:import-from #:aws-sdk/error))
(common-lisp:in-package #:aws-sdk/services/migrationhub-config/api)
(common-lisp:progn
 (common-lisp:define-condition migrationhub-config-error
     (aws-sdk/error:aws-error)
     common-lisp:nil)
 (common-lisp:export 'migrationhub-config-error))
(common-lisp:progn
 (common-lisp:defclass migrationhub-config-request
                       (aws-sdk/json-request:json-request) common-lisp:nil
                       (:default-initargs :service "migrationhub-config"
                        :api-version "2019-06-30" :host-prefix
                        "migrationhub-config" :signing-name "mgh" :global-host
                        common-lisp:nil :target-prefix
                        "AWSMigrationHubMultiAccountService" :json-version
                        "1.1"))
 (common-lisp:export 'migrationhub-config-request))
(common-lisp:defvar *error-map*
  '(("AccessDeniedException" . access-denied-exception)
    ("DryRunOperation" . dry-run-operation)
    ("InternalServerError" . internal-server-error)
    ("InvalidInputException" . invalid-input-exception)
    ("ServiceUnavailableException" . service-unavailable-exception)
    ("ThrottlingException" . throttling-exception)))
(common-lisp:progn
 (common-lisp:define-condition access-denied-exception
     (migrationhub-config-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       access-denied-exception-message)))
 (common-lisp:export
  (common-lisp:list 'access-denied-exception 'access-denied-exception-message)))
(common-lisp:deftype control-id () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defclass create-home-region-control-request common-lisp:nil
                       ((home-region :initarg :home-region :initform
                         (common-lisp:error ":home-region is required") :type
                         (common-lisp:or home-region common-lisp:null)
                         :accessor
                         struct-shape-create-home-region-control-request-home-region
                         :shape "HomeRegion" :location common-lisp:nil
                         :location-name common-lisp:nil)
                        (target :initarg :target :initform
                         (common-lisp:error ":target is required") :type
                         (common-lisp:or target common-lisp:null) :accessor
                         struct-shape-create-home-region-control-request-target
                         :shape "Target" :location common-lisp:nil
                         :location-name common-lisp:nil)
                        (dry-run :initarg :dry-run :initform common-lisp:nil
                         :type (common-lisp:or dry-run common-lisp:null)
                         :accessor
                         struct-shape-create-home-region-control-request-dry-run
                         :shape "DryRun" :location common-lisp:nil
                         :location-name common-lisp:nil))
                       (:metaclass aws-sdk/generator/shape::members-class))
 (common-lisp:defun make-create-home-region-control-request
                    (common-lisp:&rest aws-sdk/generator/shape::args)
   (common-lisp:apply #'common-lisp:make-instance
                      'create-home-region-control-request
                      aws-sdk/generator/shape::args))
 (common-lisp:export
  (common-lisp:list 'create-home-region-control-request
                    'make-create-home-region-control-request))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          create-home-region-control-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          create-home-region-control-request))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'home-region))
      (common-lisp:list
       (common-lisp:cons "HomeRegion"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'target))
      (common-lisp:list
       (common-lisp:cons "Target"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'dry-run))
      (common-lisp:list
       (common-lisp:cons "DryRun"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          create-home-region-control-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defclass create-home-region-control-result common-lisp:nil
                       ((home-region-control :initarg :home-region-control
                         :initform common-lisp:nil :type
                         (common-lisp:or home-region-control common-lisp:null)
                         :accessor
                         struct-shape-create-home-region-control-result-home-region-control
                         :shape "HomeRegionControl" :location common-lisp:nil
                         :location-name common-lisp:nil))
                       (:metaclass aws-sdk/generator/shape::members-class))
 (common-lisp:defun make-create-home-region-control-result
                    (common-lisp:&rest aws-sdk/generator/shape::args)
   (common-lisp:apply #'common-lisp:make-instance
                      'create-home-region-control-result
                      aws-sdk/generator/shape::args))
 (common-lisp:export
  (common-lisp:list 'create-home-region-control-result
                    'make-create-home-region-control-result))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          create-home-region-control-result))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          create-home-region-control-result))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'home-region-control))
      (common-lisp:list
       (common-lisp:cons "HomeRegionControl"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          create-home-region-control-result))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defclass delete-home-region-control-request common-lisp:nil
                       ((control-id :initarg :control-id :initform
                         (common-lisp:error ":control-id is required") :type
                         (common-lisp:or control-id common-lisp:null) :accessor
                         struct-shape-delete-home-region-control-request-control-id
                         :shape "ControlId" :location common-lisp:nil
                         :location-name common-lisp:nil))
                       (:metaclass aws-sdk/generator/shape::members-class))
 (common-lisp:defun make-delete-home-region-control-request
                    (common-lisp:&rest aws-sdk/generator/shape::args)
   (common-lisp:apply #'common-lisp:make-instance
                      'delete-home-region-control-request
                      aws-sdk/generator/shape::args))
 (common-lisp:export
  (common-lisp:list 'delete-home-region-control-request
                    'make-delete-home-region-control-request))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          delete-home-region-control-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          delete-home-region-control-request))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'control-id))
      (common-lisp:list
       (common-lisp:cons "ControlId"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          delete-home-region-control-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defclass delete-home-region-control-result common-lisp:nil
                       common-lisp:nil
                       (:metaclass aws-sdk/generator/shape::members-class))
 (common-lisp:defun make-delete-home-region-control-result
                    (common-lisp:&rest aws-sdk/generator/shape::args)
   (common-lisp:apply #'common-lisp:make-instance
                      'delete-home-region-control-result
                      aws-sdk/generator/shape::args))
 (common-lisp:export
  (common-lisp:list 'delete-home-region-control-result
                    'make-delete-home-region-control-result))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          delete-home-region-control-result))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          delete-home-region-control-result))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          delete-home-region-control-result))
   common-lisp:nil))
(common-lisp:deftype describe-home-region-controls-max-results ()
  'common-lisp:integer)
(common-lisp:progn
 (common-lisp:defclass describe-home-region-controls-request common-lisp:nil
                       ((control-id :initarg :control-id :initform
                         common-lisp:nil :type
                         (common-lisp:or control-id common-lisp:null) :accessor
                         struct-shape-describe-home-region-controls-request-control-id
                         :shape "ControlId" :location common-lisp:nil
                         :location-name common-lisp:nil)
                        (home-region :initarg :home-region :initform
                         common-lisp:nil :type
                         (common-lisp:or home-region common-lisp:null)
                         :accessor
                         struct-shape-describe-home-region-controls-request-home-region
                         :shape "HomeRegion" :location common-lisp:nil
                         :location-name common-lisp:nil)
                        (target :initarg :target :initform common-lisp:nil
                         :type (common-lisp:or target common-lisp:null)
                         :accessor
                         struct-shape-describe-home-region-controls-request-target
                         :shape "Target" :location common-lisp:nil
                         :location-name common-lisp:nil)
                        (max-results :initarg :max-results :initform
                         common-lisp:nil :type
                         (common-lisp:or
                          describe-home-region-controls-max-results
                          common-lisp:null)
                         :accessor
                         struct-shape-describe-home-region-controls-request-max-results
                         :shape "DescribeHomeRegionControlsMaxResults"
                         :location common-lisp:nil :location-name
                         common-lisp:nil)
                        (next-token :initarg :next-token :initform
                         common-lisp:nil :type
                         (common-lisp:or token common-lisp:null) :accessor
                         struct-shape-describe-home-region-controls-request-next-token
                         :shape "Token" :location common-lisp:nil
                         :location-name common-lisp:nil))
                       (:metaclass aws-sdk/generator/shape::members-class))
 (common-lisp:defun make-describe-home-region-controls-request
                    (common-lisp:&rest aws-sdk/generator/shape::args)
   (common-lisp:apply #'common-lisp:make-instance
                      'describe-home-region-controls-request
                      aws-sdk/generator/shape::args))
 (common-lisp:export
  (common-lisp:list 'describe-home-region-controls-request
                    'make-describe-home-region-controls-request))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          describe-home-region-controls-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          describe-home-region-controls-request))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'control-id))
      (common-lisp:list
       (common-lisp:cons "ControlId"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'home-region))
      (common-lisp:list
       (common-lisp:cons "HomeRegion"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'target))
      (common-lisp:list
       (common-lisp:cons "Target"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'max-results))
      (common-lisp:list
       (common-lisp:cons "MaxResults"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'next-token))
      (common-lisp:list
       (common-lisp:cons "NextToken"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          describe-home-region-controls-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defclass describe-home-region-controls-result common-lisp:nil
                       ((home-region-controls :initarg :home-region-controls
                         :initform common-lisp:nil :type
                         (common-lisp:or home-region-controls common-lisp:null)
                         :accessor
                         struct-shape-describe-home-region-controls-result-home-region-controls
                         :shape "HomeRegionControls" :location common-lisp:nil
                         :location-name common-lisp:nil)
                        (next-token :initarg :next-token :initform
                         common-lisp:nil :type
                         (common-lisp:or token common-lisp:null) :accessor
                         struct-shape-describe-home-region-controls-result-next-token
                         :shape "Token" :location common-lisp:nil
                         :location-name common-lisp:nil))
                       (:metaclass aws-sdk/generator/shape::members-class))
 (common-lisp:defun make-describe-home-region-controls-result
                    (common-lisp:&rest aws-sdk/generator/shape::args)
   (common-lisp:apply #'common-lisp:make-instance
                      'describe-home-region-controls-result
                      aws-sdk/generator/shape::args))
 (common-lisp:export
  (common-lisp:list 'describe-home-region-controls-result
                    'make-describe-home-region-controls-result))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          describe-home-region-controls-result))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          describe-home-region-controls-result))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input
                           'home-region-controls))
      (common-lisp:list
       (common-lisp:cons "HomeRegionControls"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'next-token))
      (common-lisp:list
       (common-lisp:cons "NextToken"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          describe-home-region-controls-result))
   common-lisp:nil))
(common-lisp:deftype dry-run () 'common-lisp:boolean)
(common-lisp:progn
 (common-lisp:define-condition dry-run-operation
     (migrationhub-config-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       dry-run-operation-message)))
 (common-lisp:export
  (common-lisp:list 'dry-run-operation 'dry-run-operation-message)))
(common-lisp:deftype error-message () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defclass get-home-region-request common-lisp:nil common-lisp:nil
                       (:metaclass aws-sdk/generator/shape::members-class))
 (common-lisp:defun make-get-home-region-request
                    (common-lisp:&rest aws-sdk/generator/shape::args)
   (common-lisp:apply #'common-lisp:make-instance 'get-home-region-request
                      aws-sdk/generator/shape::args))
 (common-lisp:export
  (common-lisp:list 'get-home-region-request 'make-get-home-region-request))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          get-home-region-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          get-home-region-request))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          get-home-region-request))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:defclass get-home-region-result common-lisp:nil
                       ((home-region :initarg :home-region :initform
                         common-lisp:nil :type
                         (common-lisp:or home-region common-lisp:null)
                         :accessor
                         struct-shape-get-home-region-result-home-region :shape
                         "HomeRegion" :location common-lisp:nil :location-name
                         common-lisp:nil))
                       (:metaclass aws-sdk/generator/shape::members-class))
 (common-lisp:defun make-get-home-region-result
                    (common-lisp:&rest aws-sdk/generator/shape::args)
   (common-lisp:apply #'common-lisp:make-instance 'get-home-region-result
                      aws-sdk/generator/shape::args))
 (common-lisp:export
  (common-lisp:list 'get-home-region-result 'make-get-home-region-result))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        (
                         (aws-sdk/generator/shape::input
                          get-home-region-result))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        (
                         (aws-sdk/generator/shape::input
                          get-home-region-result))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'home-region))
      (common-lisp:list
       (common-lisp:cons "HomeRegion"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        (
                         (aws-sdk/generator/shape::input
                          get-home-region-result))
   common-lisp:nil))
(common-lisp:deftype home-region () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defclass home-region-control common-lisp:nil
                       ((control-id :initarg :control-id :initform
                         common-lisp:nil :type
                         (common-lisp:or control-id common-lisp:null) :accessor
                         struct-shape-home-region-control-control-id :shape
                         "ControlId" :location common-lisp:nil :location-name
                         common-lisp:nil)
                        (home-region :initarg :home-region :initform
                         common-lisp:nil :type
                         (common-lisp:or home-region common-lisp:null)
                         :accessor struct-shape-home-region-control-home-region
                         :shape "HomeRegion" :location common-lisp:nil
                         :location-name common-lisp:nil)
                        (target :initarg :target :initform common-lisp:nil
                         :type (common-lisp:or target common-lisp:null)
                         :accessor struct-shape-home-region-control-target
                         :shape "Target" :location common-lisp:nil
                         :location-name common-lisp:nil)
                        (requested-time :initarg :requested-time :initform
                         common-lisp:nil :type
                         (common-lisp:or requested-time common-lisp:null)
                         :accessor
                         struct-shape-home-region-control-requested-time :shape
                         "RequestedTime" :location common-lisp:nil
                         :location-name common-lisp:nil))
                       (:metaclass aws-sdk/generator/shape::members-class))
 (common-lisp:defun make-home-region-control
                    (common-lisp:&rest aws-sdk/generator/shape::args)
   (common-lisp:apply #'common-lisp:make-instance 'home-region-control
                      aws-sdk/generator/shape::args))
 (common-lisp:export
  (common-lisp:list 'home-region-control 'make-home-region-control))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input home-region-control))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input home-region-control))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'control-id))
      (common-lisp:list
       (common-lisp:cons "ControlId"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'home-region))
      (common-lisp:list
       (common-lisp:cons "HomeRegion"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'target))
      (common-lisp:list
       (common-lisp:cons "Target"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'requested-time))
      (common-lisp:list
       (common-lisp:cons "RequestedTime"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input home-region-control))
   common-lisp:nil))
(common-lisp:progn
 (common-lisp:deftype home-region-controls ()
   '(trivial-types:proper-list home-region-control))
 (common-lisp:defun make-home-region-controls
                    (common-lisp:&rest aws-sdk/generator/shape::members)
   (common-lisp:check-type aws-sdk/generator/shape::members
                           (trivial-types:proper-list home-region-control))
   aws-sdk/generator/shape::members))
(common-lisp:progn
 (common-lisp:define-condition internal-server-error
     (migrationhub-config-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       internal-server-error-message)))
 (common-lisp:export
  (common-lisp:list 'internal-server-error 'internal-server-error-message)))
(common-lisp:progn
 (common-lisp:define-condition invalid-input-exception
     (migrationhub-config-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       invalid-input-exception-message)))
 (common-lisp:export
  (common-lisp:list 'invalid-input-exception 'invalid-input-exception-message)))
(common-lisp:deftype requested-time () 'common-lisp:string)
(common-lisp:deftype retry-after-seconds () 'common-lisp:integer)
(common-lisp:progn
 (common-lisp:define-condition service-unavailable-exception
     (migrationhub-config-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       service-unavailable-exception-message)))
 (common-lisp:export
  (common-lisp:list 'service-unavailable-exception
                    'service-unavailable-exception-message)))
(common-lisp:progn
 (common-lisp:defclass target common-lisp:nil
                       ((type :initarg :type :initform
                         (common-lisp:error ":type is required") :type
                         (common-lisp:or target-type common-lisp:null)
                         :accessor struct-shape-target-type :shape "TargetType"
                         :location common-lisp:nil :location-name
                         common-lisp:nil)
                        (id :initarg :id :initform common-lisp:nil :type
                         (common-lisp:or target-id common-lisp:null) :accessor
                         struct-shape-target-id :shape "TargetId" :location
                         common-lisp:nil :location-name common-lisp:nil))
                       (:metaclass aws-sdk/generator/shape::members-class))
 (common-lisp:defun make-target
                    (common-lisp:&rest aws-sdk/generator/shape::args)
   (common-lisp:apply #'common-lisp:make-instance 'target
                      aws-sdk/generator/shape::args))
 (common-lisp:export (common-lisp:list 'target 'make-target))
 (common-lisp:defmethod aws-sdk/generator/shape::input-headers
                        ((aws-sdk/generator/shape::input target))
   (common-lisp:append))
 (common-lisp:defmethod aws-sdk/generator/shape::input-params
                        ((aws-sdk/generator/shape::input target))
   (common-lisp:append
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'type))
      (common-lisp:list
       (common-lisp:cons "Type"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))
    (alexandria:when-let (aws-sdk/generator/shape::value
                          (common-lisp:slot-value
                           aws-sdk/generator/shape::input 'id))
      (common-lisp:list
       (common-lisp:cons "Id"
                         (aws-sdk/generator/shape::input-params
                          aws-sdk/generator/shape::value))))))
 (common-lisp:defmethod aws-sdk/generator/shape::input-payload
                        ((aws-sdk/generator/shape::input target))
   common-lisp:nil))
(common-lisp:deftype target-id () 'common-lisp:string)
(common-lisp:deftype target-type () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:define-condition throttling-exception
     (migrationhub-config-error)
     ((message :initarg :message :initform common-lisp:nil :reader
       throttling-exception-message)
      (retry-after-seconds :initarg :retry-after-seconds :initform
       common-lisp:nil :reader throttling-exception-retry-after-seconds)))
 (common-lisp:export
  (common-lisp:list 'throttling-exception 'throttling-exception-message
                    'throttling-exception-retry-after-seconds)))
(common-lisp:deftype token () 'common-lisp:string)
(common-lisp:progn
 (common-lisp:defun create-home-region-control
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key home-region target dry-run)
   (common-lisp:declare (common-lisp:ignorable home-region target dry-run))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply
                       'make-create-home-region-control-request
                       aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input
        'migrationhub-config-request aws-sdk/generator/operation::input "POST"
        "/" "CreateHomeRegionControl"))
      common-lisp:nil common-lisp:nil *error-map* "application/json")))
 (common-lisp:export 'create-home-region-control))
(common-lisp:progn
 (common-lisp:defun delete-home-region-control
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key control-id)
   (common-lisp:declare (common-lisp:ignorable control-id))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply
                       'make-delete-home-region-control-request
                       aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input
        'migrationhub-config-request aws-sdk/generator/operation::input "POST"
        "/" "DeleteHomeRegionControl"))
      common-lisp:nil common-lisp:nil *error-map* "application/json")))
 (common-lisp:export 'delete-home-region-control))
(common-lisp:progn
 (common-lisp:defun describe-home-region-controls
                    (
                     common-lisp:&rest aws-sdk/generator/operation::args
                     common-lisp:&key control-id home-region target max-results
                     next-token)
   (common-lisp:declare
    (common-lisp:ignorable control-id home-region target max-results
     next-token))
   (common-lisp:let ((aws-sdk/generator/operation::input
                      (common-lisp:apply
                       'make-describe-home-region-controls-request
                       aws-sdk/generator/operation::args)))
     (aws-sdk/generator/operation::parse-response
      (aws-sdk/api:aws-request
       (aws-sdk/generator/shape:make-request-with-input
        'migrationhub-config-request aws-sdk/generator/operation::input "POST"
        "/" "DescribeHomeRegionControls"))
      common-lisp:nil common-lisp:nil *error-map* "application/json")))
 (common-lisp:export 'describe-home-region-controls))
(common-lisp:progn
 (common-lisp:defun get-home-region ()
   (aws-sdk/generator/operation::parse-response
    (aws-sdk/api:aws-request
     (common-lisp:make-instance 'migrationhub-config-request :method "POST"
                                :path "/" :operation "GetHomeRegion"))
    common-lisp:nil common-lisp:nil *error-map*))
 (common-lisp:export 'get-home-region))
