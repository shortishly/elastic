#!/bin/sh
cd $(dirname $0)
APP=$(bin/app)

export ELASTICSEARCH_PORT_9200_TCP_ADDR=$(boot2docker ip)
export ELASTICSEARCH_PORT_9200_TCP_PORT=9200

exec erl \
     +K true \
     -boot start_sasl \
     -config dev.config \
     -name ${APP} \
     -pa deps/*/ebin \
     -pa ebin \
     -s rb \
     -s ${APP}

