(declare (unit bender-conversation))

(module bender-conversation (conversate)
    (import (except scheme string-length string-ref string-set! make-string string substring string->list list->string string-fill! write-char read-char display) (except chicken reverse-list->string print print*))
    
    ;;Entry point for conversation.                                
    (define (conversate message)        
        message))

                                    
