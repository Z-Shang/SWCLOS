Portable SWCLOS (CLOS-based Semantic Web)
======

OWL Full Processor on top of Common Lisp Object System (CLOS)

## Supported platforms

* Allegro CL Free Express Edition 10.0, 10.1
* LispWorks 6.1, 7.0, 7.1

## Other supported platforms (in progress)

* CMU Common Lisp
* Steel Bank Common Lisp
* GNU CLISP

## API changes comparing with original SWCLOS 2.0

* defConcept &rarr; def-concept
* defIndividual &rarr; def-individual
* defProperty &rarr; def-property
* defResource &rarr; def-resource
* defTriple &rarr; def-triple
* addClass &rarr; add-class
* addForm &rarr; add-form
* addObject &rarr; add-object
* addInstance &rarr; add-instance
* addTriple &rarr; add-triple
* addRdfXml &rarr; add-rdf/xml
* lasyAddRdfXml &rarr; lasy-add-rdf/xml

## OpenCyc support

The full OpenCyc OWL file (compressed) is now part of this project, as it's not available for download from Cycorp, Inc. (www.cyc.com)
