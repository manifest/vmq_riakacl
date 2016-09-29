# VerneMQ Riak ACL Plugin

[![Build Status][travis-img]][travis]

The plugin for VerneMQ that provides client authorization based on ACL stored in Riak KV.



### How To Use

Build and run the docker container

```bash
$ ./run-docker.sh
```

We need to build and enable the plugin. Execute following commands in container's shell:

```bash
$ make rel
$ vmq-admin plugin enable --name vmq_riakacl --path $(pwd)/_rel/vmq_riakacl
```

Let's allow user 'joe' connecting to the broker and publishing to the 'greetings' topic:

```bash
$ curl -fSL \
	-XPUT http://localhost:8098/types/acl_t/buckets/acl/keys/user%2Fjoe%3Acon \
	-H 'Content-Type: application/json' \
	-d '{"account_id":"joe","role":"user","limit":3,"exp":4607280000000000,"cat":1472688000000000}'
$ curl -fSL \
	-XPUT http://localhost:8098/types/acl_t/buckets/acl/keys/user%2Fjoe%3Apub%3Agreetings \
	-H 'Content-Type: application/json' \
	-d '{"account_id":"joe","qos":0,"retain":false,"exp":4607280000000000,"cat":1472688000000000}'

$ mosquitto_pub -h localhost -t greetings -m hello -i user/joe/web -u user/joe -P 123
```


### License

The source code is provided under the terms of [the MIT license][license].

[license]:http://www.opensource.org/licenses/MIT
[travis]:https://travis-ci.org/manifest/vmq_riakacl?branch=master
[travis-img]:https://secure.travis-ci.org/manifest/vmq_riakacl.png

