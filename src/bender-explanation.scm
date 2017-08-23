(declare (unit bender-explanation))

(module bender-explanation (explain)
    (import chicken scheme)
    (require-library uri-common)
    (import (prefix uri-common uri:))

    ;we need a {-# Language ViewPatterns #-} macro
    (define (explain what)
        (actually-explain (if (uri:uri-host (uri:uri-reference what))
                              (fetch what)
                              what)))
    
    (define (actually-explain what)
        what)    

    (define (fetch uri)
        uri))
