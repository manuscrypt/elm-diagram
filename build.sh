#!/bin/bash

rm -rf build
rm -rf elm-stuff
rm -rf server/node_modules

cd server
npm install
cd ..

elm-package install --yes

mkdir -p build

elm-make src/DemoProgram.elm --output build/index.html

cp build/index.html server/public/index.html
