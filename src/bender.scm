;abandon all types ye who enter here
(declare (uses bender-generation))
(import bender-generation)

(define (main args)
    (write-line (generate 'name 50)))
 
(cond-expand
    (compiling (main (command-line-arguments)))
    (else))
        
        
