# Makefile for MasterMind Haskell project.

GHC_PARAMS=
MAIN_MODULE=MasterMind
bindir=../bin
odir=-odir ../obj
hidir=-hidir ../interfaces

all: MasterMind.hs
	@echo "Building MasterMind..."
	@ghc $(GHC_PARAMS) $(MAIN_MODULE).hs -o $(bindir)/$(MAIN_MODULE) $(odir) $(hidir)
	@echo "Done."

commit: test
	git add . && git commit

test:
	@echo "Building test suite for MasterMind..."
	@ghc $(GHC_PARAMS) testSuite.hs -o $(bindir)/test $(odir) $(hidir)
	@echo "Done.\nEntering bin directory...\nRunning tests:"
	@cd $(bindir) && ./test

MasterMind.hs:
	@echo "Building $(MAIN_MODULE) library..."
	@ghc $(GHC_PARMS) $(MAIN_MODULE).hs -o $(bindir)/$(MAIN_MODULE) $(odir) $(hidir)
	@echo "Done."

clean:
	@echo "Deleting object and interface files..."
	@rm -rf ../obj ../interfaces && echo "Done."
