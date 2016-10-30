;;; manually loading the following code will crash SBCL:

(in-package :cl-user)

;;;                                         ........................
;;;                                         :                      :
;;;                                         :                    rdfsClass
;;;                                         :              ...../..:
;;;                                         :              :   /
;;; cl:standard-class -- rdf-node ----------:--------rdfs:Class
;;;                             :...........:

(defclass rdf-node (standard-class) ())

(defmethod sb-pcl:validate-superclass
    ((class rdf-node) (superclass standard-class))
  t)

(defclass rdfsClass (rdf-node) ()
  (:metaclass rdf-node))

(defclass |Class| (rdf-node) ()
  (:metaclass rdfsClass))

;; redefine rdfsClass
(defclass rdfsClass (|Class|) ()
  (:metaclass rdf-node))
