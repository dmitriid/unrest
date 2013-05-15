#/bin/sh

PWD=`pwd`

erl -pa $PWD/../como/ebin $PWD/deps/*/ebin -s reloader -s como
