;abandon all types ye who enter here
(declare (uses bender-generation))
(import bender-generation)

(define (main args)
    (write-line (suggest 'name)))

(cond-expand
    (compiling (main (command-line-arguments)))
    (else))
        
