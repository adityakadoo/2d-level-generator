TARGET =lvl_gen
ODIR =build
SRCDIR =src
SEED ?=0
TILESET ?=circuit

compile:
	ghc --make -i$(SRCDIR) $(SRCDIR)/Main.hs -outputdir $(ODIR)/ -o $(ODIR)/$(TARGET).app

debug:
	ghc --make -g -i$(SRCDIR) $(SRCDIR)/Main.hs -outputdir $(ODIR)/ -o $(ODIR)/$(TARGET).app

run:
	$(ODIR)/$(TARGET).app $(SEED) $(TILESET)

load:
	ghci src/Main.hs src/LoadShaders.hs src/TilesetLoader.hs src/WaveFuncCollapse.hs src/Randomness.hs

.PHONY: clean

clean:
	rm -f $(ODIR)/*

.PHONY: count

count:
	find . -name '*.hs' | xargs wc -l