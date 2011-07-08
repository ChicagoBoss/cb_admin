#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin -pa /Users/emiller/src/ChicagoBoss/ebin -pa /Users/emiller/src/ChicagoBoss/deps/*/ebin \
     -boot start_sasl -config boss -s reloader -s boss \
     -sname wildbill
