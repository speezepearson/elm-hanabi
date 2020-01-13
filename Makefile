ELM_FILES=$(find src/ -name '*.elm')

all: dist

dist: dist/index.html dist/bundle.js

dist/index.html: src/Native/index.html
	mkdir -p dist
	cp "$<" "$@"

dist/bundle.js: build/elm.js src/Native/init.js
	mkdir -p dist
	cat $^ > "$@"

build/elm.js: $(wildcard src/*.elm)
	mkdir -p build
	elm make --output=$@ src/Main.elm

clean:
	rm -rf build/ dist/
