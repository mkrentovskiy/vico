#!/bin/sh
exec erl +A 4 -pa ebin edit deps/*/ebin -boot start_sasl \
    -sname actions \
    -s actions \
    -s reloader
