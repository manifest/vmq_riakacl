FROM ubuntu:16.04

## -----------------------------------------------------------------------------
## Installing dependencies
## -----------------------------------------------------------------------------
ENV DEBIAN_FRONTEND noninteractive
RUN set -xe \
	&& apt-get update \
	&& apt-get -y --no-install-recommends install \
		software-properties-common \
		apt-transport-https \
		ca-certificates \
		lsb-release \
		curl \
	&& add-apt-repository -y "deb https://packages.erlang-solutions.com/ubuntu $(lsb_release -sc) contrib" \
	&& curl -vs http://packages.erlang-solutions.com/ubuntu/erlang_solutions.asc 2>&1 | apt-key add -- \
	&& apt-get update \
	&& apt-get -y --no-install-recommends install \
		erlang-nox=1:19.0-1 \
		erlang-dialyzer \
		erlang-dev \
		rsyslog \
		mosquitto-clients \
		vim-nox \
		sudo \
		less \
		make \
		git

## -----------------------------------------------------------------------------
## Installing VerneMQ
## -----------------------------------------------------------------------------
RUN set -xe \
  && VERNEMQ_URI='https://bintray.com/artifact/download/erlio/vernemq/deb/xenial/vernemq_0.14.2-1_amd64.deb' \
  && VERNEMQ_SHA1='74f24b602534a8b2295634b7d233d364d3001b19' \
  && curl -fSL -o vernemq.deb "${VERNEMQ_URI}" \
    && echo "${VERNEMQ_SHA1} vernemq.deb" | sha1sum -c - \
    && set +e; dpkg -i vernemq.deb || apt-get -y -f --no-install-recommends install; set -e \
    && rm vernemq.deb

## -----------------------------------------------------------------------------
## Installing Riak KV
## -----------------------------------------------------------------------------
## NOTE: there is no ppa for xenial, so that we're using the package for trusty
RUN set -xe \
	&& curl -fSL https://packagecloud.io/install/repositories/basho/riak/script.deb.sh | bash \
	&& perl -pi -e 's/xenial/trusty/g' /etc/apt/sources.list.d/basho_riak.list \
	&& apt-get update \
	&& apt-get -y --no-install-recommends install riak=2.1.4-1

## -----------------------------------------------------------------------------
## Configuring Riak KV
## -----------------------------------------------------------------------------
RUN set -xe \
	&& echo '[{riak_kv, [{add_paths, ["/opt/riak.modules/beam"]}]}].' > /etc/riak/advanced.config \
	&& perl -pi -e 's/storage_backend = .*/\
		storage_backend = multi\n\
		multi_backend.default = bitcask\n\
		multi_backend.bitcask.storage_backend = bitcask\n\
		multi_backend.bitcask.bitcask.data_root = \/var\/lib\/riak\/bitcask\n\
		multi_backend.memory_ttl.storage_backend = memory\n\
		multi_backend.memory_ttl.memory_backend.ttl = 1d\
		/' /etc/riak/riak.conf \
	&& perl -pi -e 's/(listener.http.internal = )127\.0\.0\.1/${1}0.0.0.0/' /etc/riak/riak.conf \
	&& perl -pi -e 's/(listener.protobuf.internal = )127\.0\.0\.1/${1}0.0.0.0/' /etc/riak/riak.conf \
	&& perl -pi -e 's/(nodename = riak)/${1}-dev-1/' /etc/riak/riak.conf \
	&& perl -pi -e 's/(log.syslog = )off/${1}on/' /etc/riak/riak.conf

## -----------------------------------------------------------------------------
## Enabling Riak Search
## -----------------------------------------------------------------------------
RUN set -xe \
	&& apt-get -y --no-install-recommends install \
		default-jre-headless \
		libsolr-java \
	&& perl -pi -e 's/(search = )off/${1}on/' /etc/riak/riak.conf

## -----------------------------------------------------------------------------
## Enabling Riak Control
## -----------------------------------------------------------------------------
RUN set -xe \
	&& perl -pi -e 's/(riak_control = )off/${1}on/' /etc/riak/riak.conf
