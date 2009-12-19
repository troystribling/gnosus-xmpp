#!/bin/sh
cd `dirname $0`
HOST=`hostname` 

dev()
{
    exec erl \
	-sname gnosus@$HOST \
	-setcookie 12345 \
	-pa $PWD/ebin $PWD/include \
	-boot start_sasl \
	-s mnesia \
	-s gnosus
}

prod() 
{
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

usage()
{
    echo "gnosus.sh prod|dev"
    exit
}

[ $# -lt 1 ] && usage

case $1 in
    dev) dev;;
    prod) prod;;
    shell) shell;;
    create_tables) create_tables;;
    *) usage;;
esac
