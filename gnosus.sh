#!/bin/sh
cd `dirname $0`
HOST=`hostname` 

dev() {
    exec erl \
	-sname gnosus@$HOST \
	-setcookie 12345 \
	-pa $PWD/ebin $PWD/include \
	-boot start_sasl \
	-s mnesia \
	-s gnosus
}

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

create_tables() {
    exec erl \
	-sname gnosus@$HOST \
	-setcookie 12345 \
	-mnesia extra_db_nodes "['ejabberd@ubuntu']" \
	-pa $PWD/ebin $PWD/deps/*/ebin \
	-boot start_sasl \
	-s mnesia \
	-s gnosus create_tables
}

create_admin() {
    exec erl \
	-sname gnosus@$HOST \
	-setcookie 12345 \
	-pa $PWD/ebin $PWD/include \
	-config $PWD/src/gnosus \
	-boot start_sasl \
	-s mnesia \
	-s gnosus create_admin
}

usage()
{
    echo "gnosus.sh prod|dev|create_tables"
    exit
}

[ $# -lt 1 ] && usage

case $1 in
    dev) dev;;
    prod) prod;;
    shell) shell;;
    create_tables) create_tables;;
    create_admin) create_admin;;
    *) usage;;
esac
