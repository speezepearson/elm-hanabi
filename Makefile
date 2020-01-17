ELM_FILES=$(find src/ -name '*.elm')

all: dist/bundle.js

dist/bundle.js: $(wildcard src/*.elm)
	mkdir -p dist
	elm make --output="$@" src/Main.elm

clean:
	rm -rf build/ dist/
