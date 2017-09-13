(in-package :gx)

(eval-when (:load-toplevel)
  (read-rdf-file #'add-rdf/xml #p"CYC:opencyc-latest.owlz"))
