#!/bin/bash

rm build/index.html
rm -rf elm-stuff/build-artifacts
elm-make src/DemoProgram.elm --output build/index.html


cp build/index.html server/public/index.html
