#!/bin/sh
cd `dirname $0`
HOST=`hostname`

prod() {
    exec erl \
	-noinput -detached \
	-sname gnosus@$HOST \
	-setcookie 12345 \
	-pa $PWD/ebin $PWD/include \
	-config $PWD/src/gnosus \
	-boot start_sasl \
	-s mnesia \
	-s gnosus
}

create_tables_and_start() {
    exec erl \
    -noinput -detached \
	-sname gnosus@$HOST \
	-setcookie 12345 \
	-mnesia extra_db_nodes "['ejabberd@$HOST']" \
	-pa $PWD/ebin $PWD/deps/*/ebin \
	-boot start_sasl \
	-s mnesia \
	-s gnosus create_tables_and_start
}

dev() {
    exec erl \
	-sname gnosus@$HOST \
	-setcookie 12345 \
	-pa $PWD/ebin $PWD/include \
	-boot start_sasl \
	-s mnesia \
	-s gnosus
}

create_tables() {
    exec erl \
	-sname gnosus@$HOST \
	-setcookie 12345 \
	-mnesia extra_db_nodes "['ejabberd@$HOST']" \
	-pa $PWD/ebin $PWD/deps/*/ebin \
	-boot start_sasl \
	-s mnesia \
	-s gnosus create_tables
}

stop() {
    pkill -f 'beam.*sname gnosus' 
}

pid() {
    pgrep -f 'beam.*sname gnosus' 
}

usage() {
    echo "gnosus.sh prod|dev|pid|create_tables|create_tables_and_start"
    exit
}

[ $# -lt 1 ] && usage

case $1 in
    dev) dev;;
    prod) prod;;
    pid) pid;;
    create_tables) create_tables;;
    create_tables_and_start) create_tables_and_start;;
    stop) stop;;
    *) usage;;
esac
