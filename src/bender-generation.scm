(declare (unit bender-generation))

(module bender-generation (generate source-text)
    (import (except scheme string-length string-ref string-set! make-string string substring string->list list->string string-fill! write-char read-char display) (except chicken reverse-list->string print print*))
    (require-extension matchable srfi-1 utf8)
    (require-library extras random-bsd srfi-69 section-combinators uni-combinators utf8-srfi-13 irregex utils stack)
    (import (prefix extras ext:) (prefix random-bsd rnd:) (prefix srfi-69 ht:) (prefix section-combinators sc:) (prefix uni-combinators un:) (prefix utf8-srfi-13 str:) (prefix irregex rg:) (prefix utils ut:) (prefix stack st:))

    ;;Entry point for text generation.                                
    (define (generate what max-chars)        
        (capitalize (cond ((eq? what 'name) (generate-name max-chars))
                          ((eq? what 'description) (generate-description max-chars))
                          (else "I know nothing about that yet."))))

    ;;Names are generated according to the following patterns:
    ;;<adjective> [, <adjective>] <noun>       
    ;;[<adjective>] <noun> [who | that] [<adverb>] <verb> [<adjective>] [<noun>] [<other>]              
    (define (generate-name max-chars)      
        (if (should-happen 60) (simple-name max-chars) (complex-name max-chars)))                          

    ;;Upcases the first letter in a string.
    (define (capitalize name)
        (string-append (str:string-upcase (str:string-take name 1)) (str:string-drop name 1)))            

    ;;Chance of an event occuring out of 100 times.            
    (define (should-happen out-100)
        (<= (rnd:random-fixnum 100) out-100))                
    
    ;;A "simple name" is a string containing <adjective> [, <adjective>] <noun>.
    (define (simple-name max-chars)                                    
        (rg:irregex-replace ",([^,]*)$" (str:string-join (make-name '() (cat-symbol-should-happen '(adjectives adjectives nouns) '(100 40 100)) max-chars) ", ") "" 1))

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
            (let* ((class (ht:hash-table-ref grammatical-classes key-word))                       
                   (sizes (list->vector (filter (rs <= (- max-chars 1)) (ht:hash-table-keys class))))
                   (total-sizes (vector-length sizes)))
                (if (= total-sizes 0)
                    #f
                    (let ((words (ht:hash-table-ref class (vector-ref sizes (rnd:random-fixnum (- total-sizes 1))))))
                        (vector-ref words (rnd:random-fixnum (- (vector-length words) 1))))))))                    

    ;;Descriptions are markov chains.        
    (define (generate-description max-chars)
        (let ((keys (list->vector (ht:hash-table-keys source-text))))
            (let describe ((text '())
                           (max-chars max-chars)                            
                           (prefix (vector-ref keys (rnd:random-fixnum (vector-length keys)))))
                (let* ((possible-words (list->vector (ht:hash-table-ref/default source-text prefix '())))
                       (word (if (= (vector-length possible-words) 0) #f (vector-ref possible-words (rnd:random-fixnum (vector-length possible-words))))))  
                    (if (or (eq? word #f) (> (string-length word) (- max-chars 1)))
                        (adjust (str:string-join (reverse text) " ") max-chars)                        
                        (describe (cons word text) (- max-chars (string-length word) 1) (string-append (second (str:string-tokenize prefix)) " " word)))))))                            
    
    ;;Balances brackets, adding a smiling face in case of an odd number of brackets.
    (define (adjust text remaining-chars)
        (let loop ((positions (unbalanced text))
                   (text text)          
                   (count 1))                                        
            (match positions        
                (() text)
                ((single) (if (and (odd? count) (> remaining-chars 1))
                              (insert-smiley text single)
                              (insert-bracket text single count)))
                ((p . ositions) (loop ositions (insert-bracket text p count) (+ count 1))))))                            
                        
    (define (insert-smiley text position)                
        (if (char=? #\) (string-ref text position))
            (str:string-replace text " :" position position)            
            (str:string-replace text ":( " position (+ position 1))))                    

    (define (insert-bracket text position count)                            
        (if (odd? count)
            (str:string-replace text "(" position (+ position 1))
            (str:string-replace text ")" position (+ position 1))))                

    ;;stack doesnt have a persistent interface either :(
    (define (unbalanced text)
        (do ((stack (st:make-stack))
             (i 0 (+ i 1)))
            ((= i (string-length text)) (reverse (st:stack->list stack)))
            (let ((char (string-ref text i)))
                (if (char=? #\( char)
                    (st:stack-push! stack i)
                    (if (char=? #\) char)
                        (if (or (st:stack-empty? stack) (char=? #\) (string-ref text (st:stack-peek stack))))
                            (st:stack-push! stack i)
                            (st:stack-pop! stack)))))))

    ;;Point free helpers.
    (define rs sc:right-section)
    (define ls sc:left-section)    
    (define c un:uni) 

    ;;The database of words, grouped by grammatical class and then word size.    
    (define grammatical-classes 
        ((lambda ()            
            ;;hash-table doesnt have a persistent interface, the alternative persistent-hash-table has a bug when adding existing keys
            (define (group-by-size words hash-table)
                (match words
                    ;;so we can use random indexes later on     
                    (() (begin (ht:hash-table-walk hash-table (lambda (key value) (ht:hash-table-set! hash-table key (list->vector value))))
                               hash-table))
                    ((w . ords) (begin (ht:hash-table-update!/default hash-table (string-length w) (ls cons w) (list w))
                                       (group-by-size ords hash-table))))) 
            (ht:alist->hash-table (list (cons 'adjectives (group-by-size (ext:read-lines "data/adjectives") (ht:make-hash-table)))
                                        (cons 'nouns (group-by-size (ext:read-lines "data/nouns") (ht:make-hash-table)))
                                        (cons 'plural-nouns (group-by-size (ext:read-lines "data/plural-nouns") (ht:make-hash-table)))
                                        (cons 'verbs (group-by-size (ext:read-lines "data/verbs") (ht:make-hash-table)))
                                        (cons 'adverbs (group-by-size (ext:read-lines "data/adverbs") (ht:make-hash-table)))
                                        (cons 'other (group-by-size (ext:read-lines "data/other") (ht:make-hash-table))))))))                     
    ;;The input text to build a markov chain.
    (define source-text             
        (let group-by-prefix ((words (str:string-tokenize (ut:read-all "data/sep-alice-grim")))                              
                              (hash-table (ht:make-hash-table)))
            (match words                
                ((w o r . ds) (let ((key (string-append w " " o)))                                                                    
                                (begin
                                    (ht:hash-table-update!/default hash-table key (ls cons r) (list r))  
                                    (group-by-prefix (cons o (cons r ds)) hash-table))))
                (_ hash-table)))))                                
