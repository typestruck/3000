;abandon all types ye who enter here

(declare (uses bender-generation bender-conversation))
(require-library uri-common spiffy intarweb spiffy-uri-match intarweb utf8-srfi-13 medea)
(import uri-common spiffy intarweb spiffy-uri-match utf8-srfi-13 medea)
(import (prefix bender-generation gen:) (prefix bender-conversation con:))

(define my-custom-routes
    `(((/ "generate")
       (GET ,(lambda _
                (let* ((params (uri-query (request-uri (current-request))))
                       (type (string->symbol (string-downcase (alist-ref 'type params))))
                       (chars (string->number (string-downcase (alist-ref 'chars params))))
                       (response (cond ((eq? type 'description) (gen:generate 'description chars))
                                       ((eq? type 'name) (gen:generate 'name chars)))))
                      (send-response status: 'ok body: (json->string response) headers: '((content-type #(text/json ((charset . "utf-8"))))))))))))         

(define (main args)       
    (vhost-map `((".*" . ,(uri-match/spiffy my-custom-routes))))
    (parameterize ((server-port 1337)                                  
                   (default-response-headers '((content-type #(text/json ((charset . "utf-8"))) (accept-charset utf-8)))))     
        (start-server)))

(cond-expand
    (compiling (main (command-line-arguments)))
    (else))
