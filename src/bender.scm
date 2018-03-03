;abandon all types ye who enter here

(declare (uses bender-generation bender-conversation))
(require-library uri-common spiffy intarweb)
(import uri-common spiffy intarweb)
(import (prefix bender-generation gen:) (prefix bender-conversation con:))

(define (handle-greeting continue)
    (let* ((uri (request-uri (current-request))))
        (if (equal? (uri-path uri) '(/ "greeting"))
            (send-response status: 'ok body: "<h1>Hello!</h1>")
            (continue))))

(define (main args)       
        (vhost-map `(("localhost" . ,handle-greeting)))
        (start-server))                            

(cond-expand
    (compiling (main (command-line-arguments)))
    (else))
