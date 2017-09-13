;;;-*- Mode: common-lisp; syntax: common-lisp; package: asdf; base: 10 -*-
;;;
;;;; SWCLOS: A Semantic Web Processor on CLOS
;;;
;;; IT Program Project in Japan: 
;;:    Building Operation-Support System for Large-scale System using IT.
;;;
;;; This code is written by Seiji Koide at Galaxy Express Corporation, Japan,
;;; for the realization of the MEXT IT Program in Japan.
;;;
;;; Copyright (c) 2003, 2004, 2006 by Galaxy Express Corporation
;;; 
;;; Copyright (c) 2007, 2008, 2009 Seiji Koide
;;;
;;; Copyright (c) 2016  University of Bologna (Author: Chun Tian)

;;; ASDF system definition.
;;; This file must be used without compiling.

(defpackage gx-system (:use :common-lisp :asdf))  
 
(in-package :gx-system)

(defvar *swclos-directory*
  (make-pathname :host (pathname-host *load-truename*)
                 :device (pathname-device *load-truename*)
                 :directory (pathname-directory *load-truename*)))

(eval-when (:execute)
  (setf (logical-pathname-translations "SWCLOS")
        `(("**;*.*"
           ,(make-pathname
              :host (pathname-host *swclos-directory*)
              :device (pathname-device *swclos-directory*)
              :directory (append (pathname-directory *swclos-directory*)
                                 (list :wild-inferiors))
              :name :wild
              :type :wild)))))

(defvar *owl-directory*
  (merge-pathnames (make-pathname :directory (append (pathname-directory *swclos-directory*)
                                                     (list "OWL")))
                   *swclos-directory*))

(eval-when (:execute)
  (setf (logical-pathname-translations "OWL")
        `(("**;*.*"
           ,(make-pathname
              :host (pathname-host *owl-directory*)
              :device (pathname-device *owl-directory*)
              :directory (append (pathname-directory *owl-directory*)
                                 (list :wild-inferiors))
              :name :wild
              :type :wild)))))

(defsystem :swclos
    :name "SWCLOS"
  :author "Seiji Koide <SeijiKoide@aol.com>"
  :maintainer "Chun Tian (binghe) <binghe.lisp@gmail.com>"
  :version "2.0.0"
  :licence "SWCLOS"
  :description "SWCLOS is an OWL Full processor on top of CLOS."
  :long-description "This code is written at Galaxy Express Corporation, Japan, for the realization of the MEXT IT Program in Japan, and is maintained by Seiji Koide."
  :depends-on (:puri :flexi-streams :closer-mop :named-readtables :gzip-stream)
  :default-component-class cl-source-file.cl
  :components
  ((:module "RDF"
    :components
    ((:file "packages")
     (:file "Utils"        :depends-on ("packages"))
     (:file "RdfIO"        :depends-on ("packages"))
     (:file "IRI"          :depends-on ("packages"))
     (:file "Xml"          :depends-on ("packages"))
     (:file "rdferror"     :depends-on ("Utils"))
     (:file "NameSpace"    :depends-on ("IRI"))
     (:file "Literal"      :depends-on ("Utils" "Xml"))
     (:file "RDFShare"     :depends-on ("RdfIO" "NameSpace"))
     (:file "RdfParser"    :depends-on ("NameSpace" "RDFShare"))
     (:file "RdfReader"    :depends-on ("RdfParser"))
     (:file "node0"        :depends-on ("IRI"))
     (:file "node"         :depends-on ("node0"))))
   (:module "RDFS"         :depends-on ("RDF")
    :components
    ((:file "SlotDef")
     (:file "RDFboot0"     :depends-on ("SlotDef"))
     (:file "RDFboot1"     :depends-on ("RDFboot0"))
     (:file "RDFboot"      :depends-on ("RDFboot1"))
     (:file "DomainRange"  :depends-on ("RDFboot"))
     (:file "RdfsKernel"   :depends-on ("SlotDef" "RDFboot"))
     (:file "GxType0"      :depends-on ("SlotDef" "RDFboot"))
     (:file "GxType"       :depends-on ("GxType0"))
     (:file "RdfsObjects"  :depends-on ("RDFboot" "GxType"))
     (:file "GxForwardRef" :depends-on ("GxType" "RdfsObjects" "DomainRange" "RdfsKernel"))
     (:file "RdfsCore"     :depends-on ("DomainRange" "RdfsObjects" "RdfsKernel"))
     (:file "gxutils"      :depends-on ("RdfsCore"))
     (:file "rdfwriter"    :depends-on ("gxutils" "GxForwardRef"))))
   (:module "OWL"          :depends-on ("RDFS")
    :components
    ((:file "owlerror")
     (:file "owlkernel")
     (:file "owlsamedifferent" :depends-on ("owlkernel"))
     (:file "owlequivalentdisjoint" :depends-on ("owlkernel"))
     (:file "NNF")
     (:file "tunify")
     (:file "subsume"      :depends-on ("NNF" "tunify"))
     (:file "OWL"          :depends-on ("subsume" "owlkernel"))))
   (:module "NTriple"      :depends-on ("RDFS")
    :components
    ((:file "ntriple")
     (:file "ntparser")
     (:file "ntwriter")))))

(in-package #:cl-user)
(format t "~%=========== System Description ================")
(describe (asdf:find-system :swclos))
(format t "===============================================~%")
(format t "~%;;To compile, execute these forms:~%~s"
  '(asdf:operate 'asdf:compile-op :swclos))

(format t "~%;;To load, execute these forms:~%~s"
  '(asdf:operate 'asdf:load-op :swclos))
