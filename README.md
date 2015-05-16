# Elastic

Elastic is a minimal [Erlang](http://erlang.org) API to
[Elasticsearch](https://www.elastic.co/products/elasticsearch)
packaged to [link](https://docs.docker.com/userguide/dockerlinks/)
directly to the official
[elasticsearch](https://registry.hub.docker.com/_/elasticsearch/)
[docker](https://registry.hub.docker.com) image.

## Building

Elastic uses [erlang.mk](https://github.com/ninenines/erlang.mk). To build run:

```
make
```

[![Build Status](https://travis-ci.org/shortishly/elastic.svg)](https://travis-ci.org/shortishly/elastic)

## Quick start

```shell
docker-compose start
./start-dev.sh
```

To index a document:

```erlang
elastic:index_document(hourly, "stats", "{}").
{ok,#{<<"_id">> => <<"AU1cyBfR_TWT0pl8At7u">>,
      <<"_index">> => <<"logstash-2015.05.16.12">>,
      <<"_type">> => <<"stats">>,
      <<"_version">> => 1,
      <<"created">> => true}}
```

## Environment

To support linking via Docker the following environment variables are
used by default to connect the elasticsearch instance.

|Parameter                           |Description|Default         |
|------------------------------------|-----------|----------------|
|ELASTICSEARCH\_PORT\_9200\_TCP\_ADDR|hostname   |$(bootdocker ip)|
|ELASTICSEARCH\_PORT\_9200\_TCP\_PORT|port       |9200            |



