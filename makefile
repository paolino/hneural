
clean:
	rm -f *.o 
	rm -f *.hi
git:
	git add Data/Neural/*.hs
	git add *.hs
	git add hneural.cabal 
	git add LICENSE
	git add makefile
	git commit
	git push
	
doc:
	haddock -o docs -h *.hs	
edit:
	gvim Data/Neural/*.hs *.hs hneural.cabal LICENSE makefile

