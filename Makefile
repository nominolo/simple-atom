.PHONY: default clean all print-dist test

default: all

-include config.mk

TOP        ?= .
HC         ?= ghc
HADDOCK    ?= haddock

DIST       ?= $(TOP)/dist

CABAL      ?= cabal

## Compiler Library

$(DIST)/setup-config: simple-atom.cabal
	@echo ">>> ===== Configuring ======================"
	$(CABAL) configure --builddir=$(DIST) --user

$(DIST)/build//build/libHSsimple-atom-0.1.a: $(DIST)/setup-config $(wildcard *.hs)
	@echo ">>> ===== Building ========================"
	$(CABAL) build --builddir=$(DIST)

all: $(DIST)/build//build/libHSsimple-atom-0.1.a

clean:
	$(CABAL) clean --builddir=$(DIST)

print-dist:
	@echo $(DIST)