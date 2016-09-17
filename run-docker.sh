#!/bin/bash

RIAKACL_DIR='/opt/sandbox/vmq_riakacl'

function PROPS() {
	local INDEX_NAME="${1}"
	local BUCKET_OPTIONS="${2}"
	if [[ ${BUCKET_OPTIONS} ]]; then
		echo "{\"props\":{\"search_index\":\"${INDEX_NAME}\",${BUCKET_OPTIONS}}}"
	else
		echo "{\"props\":{\"search_index\":\"${INDEX_NAME}\"}}"
	fi
}

function CREATE_TYPE() {
	local HOST='http://localhost:8098'
	local SCHEMA_NAME="${1}"
	local INDEX_NAME="${1}_idx"
	local TYPE_NAME="${1}_t"
	local BUCKET_OPTIONS="${2}"
	read -r RESULT <<-EOF
		curl -fSL \
			-XPUT "${HOST}/search/schema/${SCHEMA_NAME}" \
			-H 'Content-Type: application/xml' \
			--data-binary @"${RIAKACL_DIR}/priv/riak/schemas/${SCHEMA_NAME}.xml" \
		&& curl -fSL \
			-XPUT "${HOST}/search/index/${INDEX_NAME}" \
			-H 'Content-Type: application/json' \
			-d '{"schema":"${SCHEMA_NAME}"}' \
		&& riak-admin bucket-type create ${TYPE_NAME} '$(PROPS ${INDEX_NAME} ${BUCKET_OPTIONS})' \
		&& riak-admin bucket-type activate ${TYPE_NAME}
	EOF
	echo "${RESULT}"
}

read -r DOCKER_RUN_COMMAND <<-EOF
	riak start \
	&& riak-admin wait-for-service riak_kv \
	&& mkdir -p /opt/riak.modules/beam \
	&& /usr/lib/riak/erts-5.10.3/bin/erlc -o /opt/riak.modules/beam /opt/riak.modules/src/*.erl \
	&& $(CREATE_TYPE session '"dvv_enabled":false,"allow_mult":false,"last_write_wins":true,"backend":"memory"') \
	&& $(CREATE_TYPE acl '"dvv_enabled":false,"allow_mult":false,"last_write_wins":true,"backend":"memory_ttl"')
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

