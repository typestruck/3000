# getting csc to work is like getting tar options right in the first try
csc -c -j bender-suggestion src/bender-suggestion.scm 
csc -c -j bender-explanation src/bender-explanation.scm 
csc -c src/bender.scm 
csc src/bender-suggestion.o src/bender-explanation.o src/bender.o -o bender

rm src/*.o *.import.*