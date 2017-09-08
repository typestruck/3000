(declare (unit bender-generation))

(module bender-generation (generate)
    (import chicken scheme)
    (require-extension matchable srfi-1)
    (require-library extras random-bsd persistent-hash-map section-combinators srfi-13 irregex)
    (import (prefix extras ext:) (prefix random-bsd rnd:) (prefix persistent-hash-map hm:) (prefix section-combinators sc:) (prefix srfi-13 str:) (prefix irregex rg:))    
    
    (define rs sc:right-section)    
    
    (define grammatical-classes 
        ((lambda ()            
            ;;so we can use random indexes later on     
            (define (values->array hash-map)
                (hm:map-reduce (lambda (key value new-hm) (hm:map-add new-hm key (list->vector value))) (hm:persistent-map) hash-map))
            (define (group-by-size words hash-map)
                (match words
                    (() (values->array hash-map))
                    ((w . ords) (group-by-size ords (hm:map-add hash-map (string-length w) (cons w (hm:map-ref hash-map (string-length w) '()))))))) 
            (hm:persistent-map 'adjectives (group-by-size (ext:read-lines "data/adjectives") (hm:persistent-map))
                               'nouns (group-by-size (ext:read-lines "data/nouns") (hm:persistent-map))
                               'verbs (group-by-size (ext:read-lines "data/verbs") (hm:persistent-map))
                               'other (group-by-size (ext:read-lines "data/other") (hm:persistent-map))))))
      
    (define (generate what max-chars)        
        (cond 
            ((eq? what 'name) (generate-name max-chars))
            ((eq? what 'description) (generate-description max-chars))
            (else "I know nothing about that yet.")))

    ;;Names are one of the:
    ;;[the] adjective [, and] [adjective] noun       
    ;;[the] noun [that, who] [adverb] verb [other] [adjective] [noun in plural]              
    (define (generate-name max-chars)     
        (define (cat-symbol-should-happen symbols percentages)      
            (filter symbol? (map (lambda (v p) (if (should-happen p) v #f)) symbols percentages)))
        (define (should-happen out-100)
            (<= (rnd:random-fixnum 100) out-100)) 
        (define (next-word key max-chars)    
            (let* ((class (hm:map-ref grammatical-classes key))
                   ;;- 1 so we can add space/comma
                   (sizes (list->vector (filter (rs <= (- max-chars 1)) (hm:map-keys class))))
                   (total-sizes (vector-length sizes)))
                (if (= total-sizes 0)
                    #f
                    (let ((words (hm:map-ref class (vector-ref sizes (rnd:random-fixnum (- total-sizes 1))))))
                        (vector-ref words (rnd:random-fixnum (- (vector-length words) 1)))))))    
        (define (capitalize name)
            (string-append (str:string-upcase (str:string-take name 1)) (str:string-drop name 1)))                                       
        (define (simple-name)
            (define (loop name keys max-chars)
                (let ((word (if (or (null? keys) (< max-chars 0)) #f (next-word (car keys) max-chars))))
                    (if (not word)
                        (reverse name) 
                        (loop (cons word name) (cdr keys) (- max-chars (string-length word))))))
            ;;join words with ", " and remove the last comma
            (rg:irregex-replace ",([^,]*)$" (str:string-join (loop '() (cat-symbol-should-happen '(adjectives adjectives nouns) '(100 40 100)) max-chars) ", ") "" 1))
        (capitalize (if (should-happen 100) 
                        (simple-name)
                        'complex-name)))
    
    ;;Descriptions are markov chains.        
    (define (generate-description max-chars)
        'description))    
