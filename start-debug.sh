#/bin/sh

pushd `dirname $0` > /dev/null
PWD=`pwd`
popd > /dev/null

erl -pa $PWD/ebin $PWD/deps/*/ebin -s reloader -s unrest
