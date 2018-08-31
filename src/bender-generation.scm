(declare (unit bender-generation))

(module bender-generation (generate test-suite)
    (import (except scheme string-length string-ref string-set! make-string string substring string->list list->string string-fill! write-char read-char display) (except chicken reverse-list->string print print*))
    (require-extension matchable srfi-1 utf8)
    (require-library data-structures test section-combinators uni-combinators extras random-bsd srfi-69 utf8-srfi-13 irregex utils stack)
    (import (prefix extras ext:) (prefix data-structures ds:) (prefix random-bsd rnd:) (prefix srfi-69 ht:) (prefix utf8-srfi-13 str:) (prefix irregex rg:) (prefix test ts:) (prefix utils ut:) (prefix stack st:) (prefix section-combinators sc:) (prefix uni-combinators un:))

    (define +good-punctuation+ (list "." "!" "?" "..."))

    ;;Entry point for text generation.                                
    (define (generate what max-chars)        
        (capitalize (cond ((eq? what 'name) (generate-name max-chars))
                          ((eq? what 'description) (generate-description max-chars))
                          (else (abort "I know nothing about that yet.")))))

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
            ;; - 1 so we can add spaces later
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
                        (punctuate (adjust (str:string-trim-both (str:string-join (reverse text) " ")) max-chars) (- max-chars 1))
                        (describe (cons word text) (- max-chars (string-length word) 1) (string-append (second (str:string-tokenize prefix)) " " word)))))))                            
    
    ;;Balances brackets, adding a smiling face in case of an odd number of brackets.
    (define (adjust text remaining-chars)
        (let loop ((positions (unbalanced text))
                   (text text)          
                   (count 1))                                        
            (match positions        
                (() text)
                ((single) (if (and (odd? count) (> remaining-chars 0))
                              (insert-smiley text single)
                              (insert-bracket text single count)))
                ((p . ositions) (loop ositions (insert-bracket text p count) (+ count 1))))))       
                                       
    (define (punctuate text remaining-chars)
        (define (clean-of token baddie)
            (match baddie
                (() token)
                ((b . addie) (let ((size (string-length b)))
                                (if (= (str:string-suffix-length-ci token b) size)
                                    (str:string-drop-right token size)
                                    (clean-of token addie))))))
        (let* ((last-token (str:string-copy text (+ (str:string-index-right text #\space) 1)))
               (clean-last-token (clean-of last-token (list ";" "-" "--" ":" ",")))
               (article (find (ls str:string= clean-last-token) (list "a" "an" "the")))
               (dropped (+ (- (string-length last-token) (string-length clean-last-token)) (if article (+ (string-length article) 1) 0))) 
               (text (str:string-drop-right text dropped)) 
               (good-punctuation (list->vector (filter (c (ls >= (+ remaining-chars dropped)) string-length) +good-punctuation+))))
            (cond (article (punctuate text (+ remaining-chars dropped)))
                  ((> (vector-length good-punctuation) 0) (string-append text (vector-ref good-punctuation (rnd:random-fixnum (vector-length good-punctuation)))))
                  (else text))))

    (define (insert-smiley text position)                
        (if (char=? #\) (string-ref text position))
            (str:string-replace text ":" position position)            
            (str:string-replace text ":(" position (+ position 1))))                    

    (define (insert-bracket text position count)                            
        (if (odd? count)
            (str:string-replace text "(" position (+ position 1))
            (str:string-replace text ")" position (+ position 1))))                

    ;; Returns the indexes of an string wherein brackets are ummatched.
    ;; stack doesnt have a persistent interface either :(        
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
    ;; Point-free helpers.
    (define rs sc:right-section)
    (define ls sc:left-section)    
    (define c un:uni)

    ;;hash-table-update!/default has the bizarre behavior of calling the update function with the default value if the key is missing
    (define (sane-hash-table-update!/default hash-table key updater default)
        (if (ht:hash-table-exists? hash-table key)
            (ht:hash-table-set! hash-table key (updater (ht:hash-table-ref hash-table key)))
            (ht:hash-table-set! hash-table key default)))
        
    ;; Groups the grammatical classes into word length, so it is easier to compose names later.
    (define (group-by-size words hash-table)
        (match words
                ;;so we can use random indexes later on     
            (() (begin (ht:hash-table-walk hash-table (lambda (key value) (ht:hash-table-set! hash-table key (list->vector value))))
                    hash-table))
                ;;hash-table doesnt have a persistent interface, the alternative persistent-hash-table has a bug when adding existing keys           
            ((w . ords) (begin (sane-hash-table-update!/default hash-table (string-length w) (ls cons w) (list w))
                            (group-by-size ords hash-table))))) 
        
    ;; The database of words, grouped by grammatical class and then word size.    
    (define grammatical-classes
        (ht:alist->hash-table (list (cons 'adjectives (group-by-size (ext:read-lines "data/adjectives") (ht:make-hash-table)))
                                    (cons 'nouns (group-by-size (ext:read-lines "data/nouns") (ht:make-hash-table)))
                                    (cons 'plural-nouns (group-by-size (ext:read-lines "data/plural-nouns") (ht:make-hash-table)))
                                    (cons 'verbs (group-by-size (ext:read-lines "data/verbs") (ht:make-hash-table)))
                                    (cons 'adverbs (group-by-size (ext:read-lines "data/adverbs") (ht:make-hash-table)))
                                    (cons 'other (group-by-size (ext:read-lines "data/other") (ht:make-hash-table))))))
                                        
    ;; Groups a string by triplets, e.g., "da bin ich, genau" => (("da bin", "ich,"), ("bin ich,", "genau" ), ("ich, genau", ""))
    (define (group-by-prefix words hash-table)
        (define (update key-1 key-2 value)            
            (begin (sane-hash-table-update!/default hash-table (string-append key-1 " " key-2) (ls cons value) (list value))
                hash-table))
        (match words                
            ((w o . rds) (if (not (null? rds)) (group-by-prefix (cons o rds) (update w o (car rds))) (update w o "")))
            (_ hash-table)))

    ;; The input text used to build a markov chain, as a hash table.
    (define source-text
        (group-by-prefix (str:string-tokenize (ut:read-all "data/source-text")) (ht:make-hash-table)))        
                
    ;; Helper to compare hash tables.
    (define (hash-table-keys-values ht)
        (define (->string obj) (if (number? obj) (number->string obj) obj))
        (define (->list obj) (if (vector? obj) (vector->list obj) obj))
        (cons (ds:sort (map ->string (ht:hash-table-keys ht)) str:string<) 
            (list (ds:sort (ds:flatten (map ->list (ht:hash-table-values ht))) str:string<))))

    (define (test-suite)                
        (ts:test-group "group-by-prefix"
            (let ((test-hash-table-3 (hash-table-keys-values (ht:alist->hash-table (list (cons "da bin" "ich") (cons "bin ich" "")))))
                  (test-hash-table-4 (hash-table-keys-values (ht:alist->hash-table (list (cons "da bin" "ich,") (cons "bin ich," "genau") (cons "ich, genau" ""))))))
                (ts:test "bad input" (hash-table-keys-values (ht:make-hash-table)) (hash-table-keys-values (group-by-prefix "da bin" (ht:make-hash-table))))
                (ts:test "string 3 words" test-hash-table-3 (hash-table-keys-values (group-by-prefix (str:string-tokenize "da bin ich") (ht:make-hash-table))))
                (ts:test "string 4 words" test-hash-table-4 (hash-table-keys-values (group-by-prefix (str:string-tokenize "da bin ich, genau") (ht:make-hash-table))))))
        (ts:test-group "group-by-size"
            (let ((test-hash-table (hash-table-keys-values (ht:alist->hash-table (list (cons 2 (list "da")) (cons 3 (list "bin" "ich")))))))                
                (ts:test "2 and 3 words" test-hash-table (hash-table-keys-values (group-by-size (str:string-tokenize "da bin ich") (ht:make-hash-table))))))        
        (ts:test-group "unbalanced"
            (ts:test "unbalanced- empty string" '() (unbalanced ""))
            (ts:test "unbalanced- (" '(0) (unbalanced "("))
            (ts:test "unbalanced- ())" '(2) (unbalanced "())"))
            (ts:test "unbalanced- (((((" '(0 1 2 3 4) (unbalanced "(((((")))
        (ts:test-group "adjust"    
            (ts:test "adjust- (" ":(" (adjust "(" 1))
            (ts:test "adjust- )" ":)" (adjust ")" 10))
            (ts:test "adjust- genau)" "genau:)" (adjust "genau)" 10))
            (ts:test "adjust- )genau)" "(genau)" (adjust ")genau)" 10))
            (ts:test "adjust- (genau)" "(genau)" (adjust "(genau)" 10))
            (ts:test "adjust- (genau(" "(genau)" (adjust "(genau(" 10))
            (ts:test "adjust- (ge(nau(" "(ge)nau:(" (adjust "(ge(nau(" 10)))
        (ts:test-group "punctuate"
            (ts:test "punctuate- no remaining chars and nothing to change" "test test" (punctuate "test test" 0))
            (ts:current-test-comparator (lambda (expected actual) (any (lambda (good) (str:string= actual (string-append expected good))) +good-punctuation+)))
            (ts:test "punctuate- no remaining chars but extra punctuation" "test test" (punctuate "test test," 0))
            (ts:test "punctuate- no remaining chars but article" "test test" (punctuate "test test a" 0))           
            (ts:test "punctuate- no remaining chars but article and punctuation" "test test" (punctuate "test test a;" 0)))   
        (ts:test-exit)))            