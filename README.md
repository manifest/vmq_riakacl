# VerneMQ Riak ACL Plugin

[![Build Status][travis-img]][travis]

The plugin for VerneMQ that provides client authorization based on ACL stored in Riak KV.



### How To Use

Build and run the docker container

```bash
$ ./run-docker.sh
```

Execute following commands in container's shell:

```bash
$ make rel
$ vmq-admin plugin enable --name vmq_riakacl --path $(pwd)/_rel/vmq_riakacl
```



### License

The source code is provided under the terms of [the MIT license][license].

[license]:http://www.opensource.org/licenses/MIT
[travis]:https://travis-ci.org/manifest/vmq_riakacl?branch=master
[travis-img]:https://secure.travis-ci.org/manifest/vmq_riakacl.png

