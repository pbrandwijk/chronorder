chronorder:
	ghc Main.hs -o chronorder
	gzip -c man1/chronorder.1 > man1/chronorder.1.gz

clean:
	rm -rf *o *hi chronorder man1/chronorder.1.gz
