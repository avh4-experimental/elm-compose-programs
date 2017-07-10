#!/bin/bash

set -ex

elm-make --yes
(cd examples; elm-make ExampleSimple.elm --output simple.html)
(cd examples; elm-make ExampleFlags.elm --output flags.js)
(cd examples; elm-make ExampleComplicated.elm --output complicated.html)
