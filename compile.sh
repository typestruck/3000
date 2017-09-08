# getting csc to work is like getting tar options right in the first try
csc -c -j bender-generation src/bender-generation.scm 
csc -c src/bender.scm 
csc src/bender-generation.o src/bender.o -o bender

rm src/*.o *.import.*