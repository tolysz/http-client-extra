#!/bin/bash

rm -rf .cabal-sandbox
cabal sandbox init
cabal sandbox add-source /home/m/workHS/github/aeson
