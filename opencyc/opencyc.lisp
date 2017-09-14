(in-package :gx)

;; > (translate-logical-pathname #p"CYC:opencyc-latest.owlz")
;; #P"/Users/binghe/Lisp/SWCLOS/opencyc/opencyc-latest.owl.gz"
;; > (pathname-type *)
;; "owl.gz"

;; > (pathname-type #P"/Users/binghe/Lisp/SWCLOS/opencyc/opencyc-latest.owl.gz")
;; "gz"

(eval-when (:load-toplevel)
  (read-rdf-file #'add-rdf/xml #p"CYC:opencyc-latest.owlz"))
