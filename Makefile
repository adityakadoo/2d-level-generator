APPNAME =lvl_gen
ODIR =build
SRCDIR =src

compile:
	ghc --make -i$(SRCDIR) $(APPNAME).hs -outputdir $(ODIR)/ -o $(APPNAME).app

run:
	./$(APPNAME).app

.PHONY: clean

clean:
	rm -f $(ODIR)/*.o $(ODIR)/*.hi $(APPNAME)

.PHONY: count

count:
	find . -name '*.hs' | xargs wc -l