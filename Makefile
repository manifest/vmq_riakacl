PROJECT = vmq_riakacl
PROJECT_DESCRIPTION = VerneMQ Riak ACL Plugin
PROJECT_VERSION = 0.1.0

DEPS = \
	vmq_commons \
	riakc_pool

dep_vmq_commons = git git://github.com/erlio/vmq_commons.git 1.0.0
dep_riakc_pool = git git://github.com/manifest/riak-connection-pool.git master

SHELL_DEPS = tddreloader
SHELL_OPTS = \
	-eval 'application:ensure_all_started($(PROJECT), permanent)' \
	-s tddreloader start \
	-config rel/sys

include erlang.mk

PLUGIN_HOOKS= \
	[	{vmq_riakacl, auth_on_register, 5, []}, \
		{vmq_riakacl, auth_on_publish, 6, []}, \
		{vmq_riakacl, auth_on_subscribe, 3, []}, \
		{vmq_riakacl, on_client_offline, 1, []}, \
		{vmq_riakacl, on_client_gone, 1, []} ]
app::
	perl -pi -e "s/(]}\.)/\t,{env, [{vmq_plugin_hooks, $(PLUGIN_HOOKS)}]}\n\1/" ebin/vmq_riakacl.app
