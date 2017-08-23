;abandon all types ye who enter here
(declare (uses bender-suggestion bender-explanation))
(import bender-suggestion bender-explanation)

(define (main args)
    (write-line (suggest "name")))

(cond-expand
    (compiling (main (command-line-arguments)))
    (else))
        
