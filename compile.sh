# getting csc to work is like getting tar options right in the first try
csc -c -j bender-generation src/bender-generation.scm 
csc -c -j bender-conversation src/bender-conversation.scm  
csc -c src/bender.scm 
csc src/bender-generation.o src/bender-conversation.o src/bender.o -o bender

rm */*.o *.import.*