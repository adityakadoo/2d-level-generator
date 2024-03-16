TARGET =lvl_gen
ODIR =build
SRCDIR =src
APPDIR =app

compile:
	ghc --make -i$(SRCDIR) $(APPDIR)/Main.hs -outputdir $(ODIR)/ -o $(ODIR)/$(TARGET).app

run:
	$(ODIR)/$(TARGET).app

.PHONY: clean

clean:
	rm -f $(ODIR)/*

.PHONY: count

count:
	find . -name '*.hs' | xargs wc -l