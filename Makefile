# Where to put executable commands on 'make install'?
BIN = /usr/bin
MAN = /usr/share/man/man1

install: chronorder-target
	install -d $(BIN)
	install ./chronorder $(BIN)
	install ./man1/chronorder.1.gz $(MAN)

chronorder-target:
	ghc Main.hs -o chronorder
	gzip -c man1/chronorder.1 > man1/chronorder.1.gz

clean:
	rm -rf *o *hi chronorder man1/chronorder.1.gz
