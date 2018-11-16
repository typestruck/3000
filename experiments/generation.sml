datatype generating = Name | Description

datatype grammaticalClass = Adjective | Noun | PluralNoun | Verb | Adverb | Other

val seed = Random.rand (1, (Date.second o Date.fromTimeLocal o Time.now) ())

val ht : string list IntHashTable.hash_table = IntHashTable.mkTable (50, Domain);

(* 
Groups the grammatical classes into word length, so it is easier to compose names later.
*)
fun groupBySize words hashTable =
    case word of 
          [] => IntHashTable.map Array.fromList hashTable
        | w :: ords =>
            let val key = String.size w
                val value = HashTable.lookup key hashTable
                val _ = IntHashTable.insert hashTable (key, w :: getOpt (value, [])) 
            in groupBySize ords hashTable         
            end

fun grammaticalClassToString Adjective = "Adjective"
  | grammaticalClassToString Noun = "Noun"
  | grammaticalClassToString PluralNoun = "PluralNoun"
  | grammaticalClassToString Verb = "Verb"
  | grammaticalClassToString Adverb = "Adverb"
  | grammaticalClassToString Other = "Other"

(* 
The database of words, grouped by grammatical class and then word size.
*)
val grammaticalClasses =
    let val ht = HashTable.mkTable(HashString.hashString, op= )(6, Domain)
        fun tokenize fileName =
            let fun go = String.tokens Char.isSpace o TextIO.inputAll o TextIO.openIn 
                val tokens = go fileName
                val _ = TextIO.closeIn fileName
            in tokens 
            end   
        fun prepare (class, file) = (grammaticalClassToString class, groupBySize (tokenize file))
        val _ = List.app (HashTable.insert ht o prepare) [(Adjective, "../data/adjectives"), (Nouns,  "../data/nouns"), (PluralNouns, "../data/plural-nouns"), (Verbs, "../data/verbs"), (Adverbs,"../data/adverbs"), (Other, "../data/other")]
    in ht
    end

fun randomNumber max = Random.randRange (1, max) seed

(*
Chance of an event occuring out of 100 times.
*)
fun shouldHappen outOf = randomNumber 100 <= outOf

fun describe maxChars = raise Fail "not implemented"
        

(* 
Stitches together a string according to a given scheme and max-chars.        
*)
fun makeName = 



(* 
A "simple name" is a string containing <adjective> [, <adjective>] <noun>.
*)
fun simpleName maxChars =                                    
        
(* 
Names are generated according to the following patterns:
<adjective> [, <adjective>] <noun>       
[<adjective>] <noun> [who | that] [<adverb>] <verb> [<adjective>] [<noun>] [<other>] 
*)             
fun name maxChars = 
    if shouldHappen 60 then simpleName maxChars else 

(* 
Entry point for text generation.  
*)
fun generate what maxChars  = 
    case what of 
      Name => name maxChars
    | Description => describe maxChars
