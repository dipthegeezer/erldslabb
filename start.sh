#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -config $PWD/priv/erldslabb -s reloader -s erldslabb
