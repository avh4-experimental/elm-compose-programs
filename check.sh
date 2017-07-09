#!/bin/bash

set -ex

elm-make --yes
(cd examples; elm-make ExampleSimple.elm --output simple.html)
