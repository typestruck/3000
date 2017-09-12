(declare (unit bender-generation))

(module bender-generation (generate)
    (import chicken scheme)
    (require-extension matchable srfi-1)
    (require-library extras random-bsd persistent-hash-map section-combinators uni-combinators srfi-13 irregex utils)
    (import (prefix extras ext:) (prefix random-bsd rnd:) (prefix persistent-hash-map hm:) (prefix section-combinators sc:) (prefix uni-combinators un:) (prefix srfi-13 str:) (prefix irregex rg:) (prefix irregex ut:))    
    
    ;;Entry point for text generation.                                
    (define (generate what max-chars)        
        (cond ((eq? what 'name) (generate-name max-chars))
              ((eq? what 'description) (generate-description max-chars))
              (else "I know nothing about that yet.")))
        
    ;;Names are generated according to the following patterns:
    ;;<adjective> [, <adjective>] <noun>       
    ;;[<adjective>] <noun> [who | that] [<adverb>] <verb> [<adjective>] [<noun>] [<other>]              
    (define (generate-name max-chars)      
        (capitalize (if (should-happen 60) (simple-name max-chars) (complex-name max-chars))))  
        
    ;;Upcases the first letter in a string.
    (define (capitalize name)
        (string-append (str:string-upcase (str:string-take name 1)) (str:string-drop name 1)))                                 
    
    ;;Chance of an event occuring out of 100 times.            
    (define (should-happen out-100)
        (<= (rnd:random-fixnum 100) out-100))

    ;;A "simple name" is a string containing <adjective> [, <adjective>] <noun>.
    (define (simple-name max-chars)                                    
        (rg:irregex-replace ",([^,]*)$" (str:string-join (make-name '() (cat-symbol-should-happen '(adjectives adjectives nouns) '(100 40 100))max-chars) ", ") "" 1))    

    ;;A "complex name" is a string containing [<adjective>] <noun> [who | that] [<adverb>] <verb> [<adjective>] [<noun>] [<other>].       
    (define (complex-name max-chars)
        (str:string-join (make-name '() (cat-symbol-should-happen (list 'adjectives 'nouns (if (should-happen 70) "who" "that") 'adverbs 'verbs 'adjectives 'plural-nouns 'other) '(20 100 100 20 100 10 30 5)) max-chars) " "))

    ;;Given a list, returns values according to their chance of occuring.
    (define (cat-symbol-should-happen values percentages)      
        (filter (c not (rs eq? #f)) (map (lambda (v p) (if (should-happen p) v #f)) values percentages))) 

    ;;Stitches together a string according to a given scheme and max-chars.        
    (define (make-name name keys max-chars)
        (let ((word (if (or (null? keys) (< max-chars 0)) #f (next-word (car keys) max-chars))))
            (if (not word)
                (reverse name) 
                (make-name (cons word name) (cdr keys) (- max-chars (string-length word))))))            

    ;;If key-word is a string, returns it verbatim; otherwise looks up a word in grammatical-classes key-word who fits into max-chars.
    (define (next-word key-word max-chars)                
        (if (string? key-word)
            ;;- 1 so we can add spaces later
            (if (< (- max-chars 1) (string-length key-word))
                #f
                key-word)
            (let* ((class (hm:map-ref grammatical-classes key-word))                       
                   (sizes (list->vector (filter (rs <= (- max-chars 1)) (hm:map-keys class))))
                   (total-sizes (vector-length sizes)))
                (if (= total-sizes 0)
                    #f
                    (let ((words (hm:map-ref class (vector-ref sizes (rnd:random-fixnum (- total-sizes 1))))))
                        (vector-ref words (rnd:random-fixnum (- (vector-length words) 1))))))))    

    ;;Descriptions are markov chains.        
    (define (generate-description max-chars)
        source-text)    

    ;;Point free helpers.
    (define rs sc:right-section)    
    (define c un:uni)        

    ;;The database of words, grouped by grammatical class and then word size.    
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
                                'plural-nouns (group-by-size (ext:read-lines "data/plural-nouns") (hm:persistent-map))
                                'verbs (group-by-size (ext:read-lines "data/verbs") (hm:persistent-map))
                                'adverbs (group-by-size (ext:read-lines "data/adverbs") (hm:persistent-map))
                                'other (group-by-size (ext:read-lines "data/other") (hm:persistent-map)))))                                                                
     (define source-text 
        (let group-by-prefix ((words (string-tokenize (ut:read-all "data/sep-alice-inferno")))
                              (hash-map (hm:persistent-map)))
            (match words                
                ((w . o . r . ds) (group-by-prefix (apply cons o r ds) (hm:map-add hash-map (string-append w o) (cons r (hm:map-ref hash-map (string-append w o) '())))))
                (_ hash-map))))))                           
