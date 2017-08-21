;;abandon all types ye who enter here
(declare (uses bender-suggestion))

(cond-expand
    ((not compiling) (load "bender-suggestion.scm"))
    (else))

(define (main args)
   (write-line (suggest "name")))
