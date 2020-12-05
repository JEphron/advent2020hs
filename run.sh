#!/bin/fish
ghcid --command "stack ghci advent2020:lib advent2020:test:advent2020-test --ghci-options=-fobject-code" --test "main"
