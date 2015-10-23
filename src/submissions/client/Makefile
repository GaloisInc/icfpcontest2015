.PHONY: all clean

all: dist/app.js

clean:
	rm -rf dist

node_modules: package.json
	npm install

dist/app.js: $(shell find lib) webpack.config.js
	node_modules/.bin/webpack --config webpack.config.js
