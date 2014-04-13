# Makefile for MasterMind Haskell project.

SHELL:=/bin/bash
GHC_PARAMS=
MAIN_MODULE=MasterMind
bindir=../bin
odir=../obj
hidir=../interfaces

all: mmind

test: test_mmind

mmind: create_env
	@echo "Building MasterMind..."
	ghc $(GHC_PARAMS) mmind.hs -o $(bindir)/$(MAIN_MODULE) -odir $(odir) -hidir $(hidir)
	@echo "Done."

commit: test
	git add . && git commit

test_mmind: create_env
	@echo "Building test suite for MasterMind..."
	ghc $(GHC_PARAMS) MasterMind/testSuite.hs -o $(bindir)/testMasterMind -odir $(odir) -hidir $(hidir)
	@echo -e  "Done.\nEntering bin directory...\nRunning tests fo MasterMind:"
	@cd $(bindir) && ./testMasterMind

create_env:
	@[[ -d "$(odir)" ]] || mkdir "$(odir)"
	@[[ -d "$(hidir)" ]] || mkdir "$(hidir)"

clean:
	@echo "Deleting object and interface files..."
	@rm -rf ../obj ../interfaces && echo "Done."
