#!/bin/bash

RIAKACL_DIR='/opt/sandbox/vmq_riakacl'

read -r DOCKER_RUN_COMMAND <<-EOF
	riak start \
	&& riak-admin wait-for-service riak_kv \
	&& mkdir -p /opt/riak.modules/beam \
	&& /usr/lib/riak/erts-5.10.3/bin/erlc -o /opt/riak.modules/beam /opt/riak.modules/src/*.erl \
	&& curl -fSL \
		-XPUT "http://localhost:8098/search/schema/session" \
		-H 'Content-Type: application/xml' \
		--data-binary @"${RIAKACL_DIR}/priv/riak/schemas/session.xml" \
	&& curl -fSL \
		-XPUT "http://localhost:8098/search/index/session_idx" \
		-H 'Content-Type: application/json' \
		-d '{"schema":"session"}' \
	&& riak-admin bucket-type create session_t '{"props":{"search_index":"session_idx"}}' \
	&& riak-admin bucket-type activate session_t \
	&& curl -fSL \
		-XPUT "http://localhost:8098/search/schema/acl" \
		-H 'Content-Type: application/xml' \
		--data-binary @"${RIAKACL_DIR}/priv/riak/schemas/acl.xml" \
	&& curl -fSL \
		-XPUT "http://localhost:8098/search/index/acl_idx" \
		-H 'Content-Type: application/json' \
		-d '{"schema":"acl"}' \
	&& riak-admin bucket-type create acl_t '{"props":{"search_index":"acl_idx"}}' \
	&& riak-admin bucket-type activate acl_t
EOF

docker build -t sandbox/vmq_riakacl .
docker run -ti --rm \
	-v $(pwd):${RIAKACL_DIR} \
	-v $(pwd)/priv/riak/modules:/opt/riak.modules \
	-p 8098:8098 \
	-p 8087:8087 \
	-p 8093:8093 \
	-p 8985:8985 \
	sandbox/vmq_riakacl \
	/bin/bash -c "set -x && ${DOCKER_RUN_COMMAND} && set +x && cd ${RIAKACL_DIR} && /bin/bash"

