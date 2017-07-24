.PHONY: deps clean all

SRC_DIR     := "src"
ELM_DIR     := "$(SRC_DIR)/elm"
ELM_SOURCES := $(shell find $(ELM_DIR) -name '*.elm')
HS_SOURCES  := $(shell find $(SRC_DIR) -name '*.hs')

.cabal-sandbox/bin/stream-alerts-bot: deps static/overlay.js $(HS_SOURCES)
	cabal build
	cabal install

.cabal-sandbox:
	cabal sandbox init

deps: .cabal-sandbox
	cabal install --only-dependencies

static/overlay.js: $(ELM_SOURCES)
	elm-make src/elm/Main.elm --output static/overlay.js --yes

clean:
	rm -rf elm-stuff
	rm -f static/*.js
	rm -rf .cabal-sandbox
