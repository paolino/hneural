
clean:
	rm -f *.o 
	rm -f *.hi
git:
	git add Data/Neural/*.hs
	git add reservoir.hs
	git add hneural.cabal 
	git add LICENSE
	git add makefile
	git commit
	git push
	
doc:
	haddock -o docs -h *.hs	
edit:
	gvim Data/Neural/*.hs reservoir.hs LICENSE makefile

