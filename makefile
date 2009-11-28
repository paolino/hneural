
clean:
	rm -rf *.o 
	rm -rf *.hi
	rm -f `find . -maxdepth 4 (-perm -u=x -type f) || (-name=`
git:
	git add Data/Neural/*.hs
	git add README
	git add LICENSE
	git add TODO
	git add makefile
	git add hneural.cabal
	git commit
	git push
	
doc:
	haddock -o docs -h *.hs	
edit:
	gvim *.hs README LICENSE TODO makefile hneural.cabal

