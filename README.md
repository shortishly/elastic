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
elastic:index_document(daily, "stats", jsx:encode(#{timestamp => erlang:universaltime(), message => <<"hello world">>})). 
{ok,#{<<"_id">> => <<"AU4cOmHP2oVYagdYKPh5">>,
      <<"_index">> => <<"logstash-2015.06.22">>,
      <<"_type">> => <<"stats">>,
      <<"_version">> => 1,
      <<"created">> => true}}
```

In Kibana, create a new index by selecting: "settings", followed by
"add new" index pattern. Ensure that both "index contains time-based
events" and also "use event times to create index names", with "daily"
as the index pattern interval. The "time-field name" should populate
with "timestamp" (otherwise a quick "refresh fields" should sort it).

Once the index is created, you can navigate to "Discover" and view the
indexed data in Kibana.


## Environment

To support linking via Docker the following environment variables are
used by default to connect the elasticsearch instance.

|Parameter                           |Description|Default         |
|------------------------------------|-----------|----------------|
|ELASTICSEARCH\_PORT\_9200\_TCP\_ADDR|hostname   |$(bootdocker ip)|
|ELASTICSEARCH\_PORT\_9200\_TCP\_PORT|port       |9200            |



