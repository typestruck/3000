;abandon all types ye who enter here
(declare (uses bender-generation bender-conversation))
(require-library nanomsg utf8-srfi-13 bender-generation bender-conversation)
(import (prefix bender-generation gen:) (prefix bender-conversation con:) (prefix nanomsg nm:) (prefix utf8-srfi-13 str:))

(define (main args)
    (let ((socket (nm:nn_socket 'rep)))
        (nm:nn-bind socket "tcp://0.0.0.1:10080")
        (let loop ()
            (let* ((message (nm:nn-recv socket))                   
                   (delimiter (str:string-index message char-set:whitespace))
                   (type (string->symbol (str:string-downcase (str:string-take message delimiter))))
                   (rest (str:string-skip message (+ delimiter 1))))
                (nm:nn-send socket (if (or (eqv? type 'name) (eq? type 'description)) 
                                       (gen:generate type (string->number rest))
                                       (con:conversate rest)))
                (loop)))))                    
 
(cond-expand
    (compiling (main (command-line-arguments)))
    (else))
        
        
