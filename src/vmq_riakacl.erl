%% ----------------------------------------------------------------------------
%% The MIT License
%%
%% Copyright (c) 2016 Andrei Nesterov <ae.nesterov@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to
%% deal in the Software without restriction, including without limitation the
%% rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
%% sell copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%% IN THE SOFTWARE.
%% ----------------------------------------------------------------------------

-module(vmq_riakacl).
-behaviour(auth_on_register_hook).
-behaviour(auth_on_subscribe_hook).
-behaviour(auth_on_publish_hook).
-behaviour(on_client_offline_hook).
-behaviour(on_client_gone_hook).

-include("deps/vmq_commons/include/vmq_types.hrl").

%% API
-export([
	read_configs/0,
	read_configs/1,
	read_config/2,
	unix_time_us/0,
	unix_time_us/1
]).

%% Hooks
-export([
	auth_on_register/5,
	auth_on_subscribe/3,
	auth_on_publish/6,
	on_client_offline/1,
	on_client_gone/1
]).

%% Plugin Callbacks
-export([
	start/0,
	stop/0
]).

%% Configuration
-export([
	config_files/0,
	pools/0
]).

%% Definitions
-define(APP, ?MODULE).

%% =============================================================================
%% API
%% =============================================================================

-spec read_configs() -> ok.
read_configs() ->
	Initial = config_files(),
	read_configs(Initial),
	case config_files() of
		Initial -> ok;
		Changed -> read_configs(Changed)
	end.

-spec read_configs([{atom(), binary()}]) -> ok.
read_configs(L) ->
	[read_config(App, Path) || {App, Path} <- L],
	ok.

-spec read_config(atom(), binary()) -> ok.
read_config(App, Path) ->
	_ =
		case file:consult(Path) of
			{ok, L} -> [application:set_env(App, Key, Val) || {Key, Val} <- L];
			_       -> ignore
		end,
	ok.

-spec unix_time_us() -> non_neg_integer().
unix_time_us() ->
	unix_time_us(erlang:timestamp()).

-spec unix_time_us(erlang:timestamp()) -> non_neg_integer().
unix_time_us({MS, S, US}) ->
	MS * 1000000000000 + S * 1000000 + US.

%% =============================================================================
%% Hooks
%% =============================================================================

-spec auth_on_register(peer(), subscriber_id(), username(), password(), flag()) ->
	ok
	| {ok, [auth_on_register_hook:reg_modifiers()]}
	| {error, invalid_credentials | any()}
	| next.
auth_on_register(_Peer, SubscriberId, UserName, _Password, _CleanSession) ->
	try
		do_auth_on_register(UserName, SubscriberId)
	catch T:R ->
		error_logger:info_report(
			[	{?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, erlang:get_stacktrace(), T, R},
				{username, UserName}]),
		{T, R}
	end.

-spec auth_on_subscribe(username(), subscriber_id(), [{topic(), qos()}]) ->
	ok
	| {ok, [{topic(), qos()}]}
	| {error, any()}
	| next.
auth_on_subscribe(UserName, _SubscriberId, Topics) ->
	try
		do_auth_on_subscribe(UserName, Topics)
	catch T:R ->
		error_logger:info_report(
			[	{?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, erlang:get_stacktrace(), T, R},
				{username, UserName}]),
		{T, R}
	end.

-spec auth_on_publish(username(), subscriber_id(), qos(), topic(), payload(), flag()) ->
	ok
	| {ok, payload()}
	| {ok, [auth_on_publish_hook:msg_modifier()]}
	| {error, any()}
	| next.
auth_on_publish(UserName, _SubscriberId, Qos, Topic, _Payload, IsRetain) ->
	try
		do_auth_on_publish(UserName, Topic, Qos, IsRetain)
	catch T:R ->
		error_logger:info_report(
			[	{?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, erlang:get_stacktrace(), T, R},
				{username, UserName}]),
		{T, R}
	end.

-spec on_client_offline(subscriber_id()) -> any().
on_client_offline({_MountPoint, ClientId}) ->
	try
		do_on_client_offline(ClientId)
	catch T:R ->
		error_logger:info_report(
			[	{?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, erlang:get_stacktrace(), T, R},
				{client_id, ClientId}]),
		{T, R}
	end.

-spec on_client_gone(subscriber_id()) -> any().
on_client_gone({_MountPoint, ClientId}) ->
	try
		do_on_client_offline(ClientId)
	catch T:R ->
		error_logger:info_report(
			[	{?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, erlang:get_stacktrace(), T, R},
				{client_id, ClientId}]),
		{T, R}
	end.

%% =============================================================================
%% Plugin Callbacks
%% =============================================================================

-spec start() -> ok | {error, any()}.
start() ->
	read_configs(),
	{ok, _} = application:ensure_all_started(?APP),

	try
		cleanup_sessions(broker_id())
	catch T:R ->
		error_logger:info_report(
			[	{?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, erlang:get_stacktrace(), T, R},
				{broker_id, broker_id()}]),
		{T, R}
	end.

-spec stop() -> ok | {error, any()}.
stop() ->
	try
		cleanup_sessions(broker_id()),
		application:stop(?APP)
	catch T:R ->
		error_logger:info_report(
			[	{?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, erlang:get_stacktrace(), T, R},
				{broker_id, broker_id()}]),
		{T, R}
	end.

%% =============================================================================
%% Configuration
%% =============================================================================

-spec config_files() -> [{atom(), binary()}].
config_files() ->
	Default = [{?APP, "/etc/vernemq/riakacl.conf"}],
	[	{App, list_to_binary(Val)}
		|| {App, Val} <- application:get_env(?APP, ?FUNCTION_NAME, Default)].

-spec pools() -> [map()].
pools() ->
	Default =
	[	#{name => default,
			size => 10,
			connection =>
			#{host => "localhost",
				port => 8087,
				options => [queue_if_disconnected] }} ],
	application:get_env(?APP, ?FUNCTION_NAME, Default).

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec do_auth_on_register(username(), subscriber_id()) -> ok.
do_auth_on_register(UserName, {_MountPoint, ClientId}) ->
	[Role, AccountId] = parse_username(UserName),
	AclValue = decode_obj(riakc_obj:get_value(vmq_riakacl_acl:get(con, AccountId))),
	#{<<"role">> := AclRole, <<"exp">> := AclExp, <<"limit">> := AclLimit} = AclValue,
	SessionDocsCount = length(vmq_riakacl_session:list(#{account_id => AccountId})),

	match_role(Role, AclRole),
	match_exp(AclExp, unix_time_us()),
	match_limit(SessionDocsCount, AclLimit),

	Obj =
		riakc_obj:new(vmq_riakacl_session:bucket(),
			undefined,
			jsx:encode(
				#{account_id => AccountId,
					client_id => ClientId,
					broker_id => broker_id(),
					cat => vmq_riakacl:unix_time_us() }),
			vmq_riakacl_session:content_type()),

	riakc_pool:query(default, put, [Obj]),
  auth_on_register_success_result().

-spec do_auth_on_subscribe(username(), list()) -> {ok, [{topic(), qos()}]}.
do_auth_on_subscribe(UserName, Topics) ->
	[_Role, AccountId] = parse_username(UserName),

	NewTopics =
		lists:foldl(
			fun ({Topic, Qos}, Acc) ->
					AclValue = decode_obj(riakc_obj:get_value(vmq_riakacl_acl:get(sub, AccountId, Topic))),
					#{<<"exp">> := AclExp, <<"qos">> := AclQos} = AclValue,

					case check_exp(AclExp, Time = unix_time_us()) of
						true  -> [{Topic, validated_qos(AclQos, Qos)} | Acc];
						false ->
							error_logger:info_report(
								[	{?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
									{sub_key, vmq_riakacl_acl:sub_key(AccountId, Topic), {nomatch_exp, AclExp, Time}}]),
							Acc
					end
			end,
			[],
			Topics),

	{ok, NewTopics}.

-spec do_auth_on_publish(username(), topic(), qos(), flag()) -> {ok, [auth_on_publish_hook:msg_modifier()]}.
do_auth_on_publish(UserName, Topic, Qos, IsRetain) ->
	[_Role, AccountId] = parse_username(UserName),
	AclValue = decode_obj(riakc_obj:get_value(vmq_riakacl_acl:get(pub, AccountId, Topic))),
	#{<<"exp">> := AclExp, <<"qos">> := AclQos, <<"retain">> := AclIsRetain} = AclValue,

	match_exp(AclExp, unix_time_us()),
	{ok, [{qos, validated_qos(AclQos, Qos)}, {retain, validated_retain_flag(AclIsRetain, IsRetain)}]}.

-spec do_on_client_offline(client_id()) -> any().
do_on_client_offline(ClientId) ->
	case vmq_riakacl_session:list(#{client_id => ClientId}) of
		[Key | _Rest] -> riakc_pool:query(default, delete, [vmq_riakacl_session:bucket(), Key]);
		[]            -> error({missing_session, ClientId})
	end.

-spec cleanup_sessions(binary()) -> ok.
cleanup_sessions(BrokerId) ->
	Args =
		[	{search, <<"vmq-riakacl-session_idx">>, <<"broker_id:", BrokerId/binary>>},
			[	{map, {modfun, vmq_riakacl_mrh_session, key}, ignore, false},
				{reduce, {modfun, vmq_riakacl_mrh_session, remove}, ignore, false} ]],

	_ =
		case catch riakc_pool:query(default, mapred, Args) of
			{ok, _}                    -> ok;
			{error, Reason}            -> exit(Reason);
			{'EXIT', Reason}           -> exit(Reason);
			Else                       -> exit({bad_return_value, Else})
	end.

-spec auth_on_register_success_result() -> ok | next.
auth_on_register_success_result() ->
	application:get_env(?APP, ?FUNCTION_NAME, ok).

-spec match_role(binary(), binary()) -> ok.
match_role(Role, Role) ->
	ok;
match_role(Role, AclRole) ->
	error({nomatch_role, Role, AclRole}).

-spec check_exp(integer(), integer()) -> boolean().
check_exp(AclExp, Time) when AclExp < Time ->
	false;
check_exp(_, _) ->
	true.

-spec match_exp(integer(), integer()) -> ok.
match_exp(AclExp, Time) ->
	case check_exp(AclExp, Time) of
		true  -> ok;
		false -> error({nomatch_exp, AclExp, Time})
	end.

-spec match_limit(integer(), integer()) -> ok.
match_limit(SessionDocsCount, AclLimit) when AclLimit =< SessionDocsCount ->
	error({nomatch_limit, SessionDocsCount, AclLimit});
match_limit(_, _) ->
	ok.

-spec parse_username(binary()) -> list().
parse_username(UserName) ->
	binary:split(UserName, <<$/>>).

-spec decode_obj(binary()) -> map().
decode_obj(Value) ->
	jsx:decode(Value, [return_maps]).

-spec validated_qos(integer(), integer()) -> integer().
validated_qos(AclQos, Qos) when Qos < AclQos ->
	Qos;
validated_qos(AclQos, _) ->
	AclQos.

-spec validated_retain_flag(boolean(), boolean()) -> boolean().
validated_retain_flag(AclIsRetain = false, _IsRetain) ->
	AclIsRetain;
validated_retain_flag(_, IsRetain) ->
	IsRetain.

-spec broker_id() -> binary().
broker_id() ->
	atom_to_binary(node(), utf8).

%% =============================================================================
%% tests
%% =============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(ACCOUNT_ID, <<"Hash">>).
-define(ACL_BUCKET, {<<"vmq-riakacl-acl_t">>, <<"vmq-riakacl-acl">>}).
-define(CAT_IN_FUTURE, 14745453269685910).
-define(CON_KEY, <<"Hash:con">>).
-define(EXP_IN_FUTURE, 14745453269685910).
-define(EXP_IN_PAST, 1).
-define(LIMIT, 10).
-define(QOS, 1).
-define(PUB_KEY, <<"Hash:pub:TopicName">>).
-define(PUB_KEY2, <<"Hash:pub:Name1/Name2">>).
-define(IS_RETAIN, false).
-define(SESSION_BUCKET, {<<"vmq-riakacl-session_t">>, <<"vmq-riakacl-session">>}).
-define(SUB_KEY, <<"Hash:sub:TopicName">>).
-define(UNKNOWN_ACCOUNT_ID, <<"UnknownAccountId">>).
-define(UNKNOWN_KEY, <<"UnknownAccountId:con">>).

%% =============================================================================
%% Test helpers
%% =============================================================================

delete_all_keys(Bucket, Keys) ->
	delete_all_keys(Bucket, Keys, []).

delete_all_keys(_, [], Acc) ->
	{ok, lists:reverse(Acc)};
delete_all_keys(Bucket, [Key | Rest], Acc) ->
	riakc_pool:query(default, delete, [Bucket, Key]),
	delete_all_keys(Bucket, Rest, [Key | Acc]).

bucket_keys(Bucket) ->
	{ok, Keys} = riakc_pool:query(default, list_keys, [Bucket]),
	Keys.

acl_put_(Key, AccountId, Exp, Limit, Qos, IsRetain) ->
	Obj =
		riakc_obj:new(
			?ACL_BUCKET,
			Key,
			jsx:encode(
				#{account_id => AccountId,
					exp => Exp,
					cat => ?CAT_IN_FUTURE,
					qos => Qos,
					retain => IsRetain,
					role => <<"Role">>,
					limit => Limit }),
					"application/json"),
	riakc_pool:query(default, put, [Obj]),
  ok.

session_put_(AccountId, ClientId, BrokerId) ->
	Obj =
		riakc_obj:new(vmq_riakacl_session:bucket(),
			undefined,
			jsx:encode(
				#{account_id => AccountId,
					client_id => ClientId,
					broker_id => BrokerId,
					cat => vmq_riakacl:unix_time_us() }),
			vmq_riakacl_session:content_type()),

	riakc_pool:query(default, put, [Obj]),
	ok.

-spec setup() -> any().
setup() ->
	vmq_riakacl:start(),
	error_logger:tty(false).

%% =============================================================================
%% Test generators
%% =============================================================================

auth_on_register_test_() ->
	setup(),

  {"auth_on_register", [
		{"bad key", setup,
			fun()  -> acl_put_(?UNKNOWN_KEY, ?UNKNOWN_ACCOUNT_ID, ?EXP_IN_FUTURE, ?LIMIT, ?QOS, ?IS_RETAIN) end,
			fun(_) -> delete_all_keys(?ACL_BUCKET, [?UNKNOWN_KEY]) end,
			?_assertEqual({error, {bad_key, ?CON_KEY}},
				vmq_riakacl:auth_on_register(1, {"MountPoint", <<"ClientId">>}, <<"Role/Hash">>, 1, 2))
		},
		{"nomatch role", setup,
			fun()  -> acl_put_(?CON_KEY, ?ACCOUNT_ID, ?EXP_IN_FUTURE, ?LIMIT, ?QOS, ?IS_RETAIN) end,
			fun(_) -> delete_all_keys(?ACL_BUCKET, [?CON_KEY]) end,
			?_assertEqual({error, {nomatch_role, <<"BadRole">>, <<"Role">>}},
				vmq_riakacl:auth_on_register(1, {"MountPoint", <<"ClientId">>}, <<"BadRole/Hash">>, 1, 2))
		},
		{"nomatch exp", setup,
			fun() -> acl_put_(?CON_KEY, ?ACCOUNT_ID, ?EXP_IN_PAST, ?LIMIT, ?QOS, ?IS_RETAIN) end,
			fun(_) -> delete_all_keys(?ACL_BUCKET, [?CON_KEY]) end,
			?_assertMatch({error, {nomatch_exp, ?EXP_IN_PAST, _}},
				vmq_riakacl:auth_on_register(1, {"MountPoint", <<"ClientId">>}, <<"Role/Hash">>, 1, 2))
		},
		{"nomatch limit (acl restriction)", setup,
			fun() -> acl_put_(?CON_KEY, ?ACCOUNT_ID, ?EXP_IN_FUTURE, 0, ?QOS, ?IS_RETAIN) end,
			fun(_) -> delete_all_keys(?ACL_BUCKET, [?CON_KEY]) end,
			?_assertEqual({error, {nomatch_limit, 0, 0}},
				vmq_riakacl:auth_on_register(1, {"MountPoint", <<"ClientId">>}, <<"Role/Hash">>, 1, 2))
		},
		{"nomatch limit (out of session limit)", setup,
			fun() ->
				session_put_(?ACCOUNT_ID, <<"ClientId1">>, broker_id()),
				session_put_(?ACCOUNT_ID, <<"ClientId2">>, broker_id()),
				timer:sleep(1000),
				acl_put_(?CON_KEY, ?ACCOUNT_ID, ?EXP_IN_FUTURE, 1, ?QOS, ?IS_RETAIN)
			end,
			fun(_) ->
				delete_all_keys(?SESSION_BUCKET, bucket_keys(?SESSION_BUCKET)),
				delete_all_keys(?ACL_BUCKET, [?CON_KEY])
			end,
			?_assertEqual({error, {nomatch_limit, 2, 1}},
				vmq_riakacl:auth_on_register(1, {"MountPoint", <<"ClientId">>}, <<"Role/Hash">>, 1, 2))
		}
	]}.

auth_on_subscribe_test_() ->
	setup(),

  {"auth_on_subsribe", [
		{"bad key", setup,
			fun() -> acl_put_(?UNKNOWN_KEY, ?UNKNOWN_ACCOUNT_ID, ?EXP_IN_FUTURE, ?LIMIT, ?QOS, ?IS_RETAIN) end,
			fun(_) -> delete_all_keys(?ACL_BUCKET, [?UNKNOWN_KEY]) end,
			?_assertEqual({error, {bad_key, ?SUB_KEY}},
				vmq_riakacl:auth_on_subscribe(<<"Role/Hash">>, <<"ClentId">>, [{<<"TopicName">>, 0}]))
		},
		{"nomatch exp", setup,
			fun() -> acl_put_(?SUB_KEY, ?ACCOUNT_ID, ?EXP_IN_PAST, ?LIMIT, ?QOS, ?IS_RETAIN) end,
			fun(_) -> delete_all_keys(?ACL_BUCKET, [?SUB_KEY]) end,
			?_assertMatch({ok, []},
				vmq_riakacl:auth_on_subscribe(<<"Role/Hash">>, <<"ClentId">>, [{<<"TopicName">>, 0}]))
		},
		{"success path", setup,
			fun() -> acl_put_(?SUB_KEY, ?ACCOUNT_ID, ?EXP_IN_FUTURE, ?LIMIT, 1, ?IS_RETAIN) end,
			fun(_) -> delete_all_keys(?ACL_BUCKET, [?SUB_KEY]) end,
			?_assertEqual({ok, [{<<"TopicName">>, 0}]},
				vmq_riakacl:auth_on_subscribe(<<"Role/Hash">>, <<"ClentId">>, [{<<"TopicName">>, 0}]))
		},
		{"replace qos", setup,
			fun() -> acl_put_(?SUB_KEY, ?ACCOUNT_ID, ?EXP_IN_FUTURE, ?LIMIT, 1, ?IS_RETAIN) end,
			fun(_) -> delete_all_keys(?ACL_BUCKET, [?SUB_KEY]) end,
			?_assertEqual({ok, [{<<"TopicName">>, 1}]},
				vmq_riakacl:auth_on_subscribe(<<"Role/Hash">>, <<"ClentId">>, [{<<"TopicName">>, 2}]))
		}
	]}.

auth_on_publish_test_() ->
	setup(),

  {"auth_on_publish", [
		{"bad key", setup,
			fun() -> acl_put_(?UNKNOWN_KEY, ?UNKNOWN_ACCOUNT_ID, ?EXP_IN_FUTURE, ?LIMIT, ?QOS, ?IS_RETAIN) end,
			fun(_) -> delete_all_keys(?ACL_BUCKET, [?UNKNOWN_KEY]) end,
			?_assertEqual({error, {bad_key, ?PUB_KEY}},
				vmq_riakacl:auth_on_publish(<<"Role/Hash">>, <<"SubscriberId">>, 1, <<"TopicName">>, <<"Payload">>, false))
		},
		{"nomatch exp", setup,
			fun() -> acl_put_(?PUB_KEY, ?ACCOUNT_ID, ?EXP_IN_PAST, ?LIMIT, ?QOS, ?IS_RETAIN) end,
			fun(_) -> delete_all_keys(?ACL_BUCKET, [?PUB_KEY]) end,
			?_assertMatch({error, {nomatch_exp, ?EXP_IN_PAST, _}},
				vmq_riakacl:auth_on_publish(<<"Role/Hash">>, <<"SubscriberId">>, 1, <<"TopicName">>, <<"Payload">>, false))
		},
		{"success path", setup,
			fun() -> acl_put_(?PUB_KEY, ?ACCOUNT_ID, ?EXP_IN_FUTURE, ?LIMIT, 1, ?IS_RETAIN) end,
			fun(_) -> delete_all_keys(?ACL_BUCKET, [?PUB_KEY]) end,
			?_assertEqual({ok, [{qos, 1}, {retain, false}]},
				vmq_riakacl:auth_on_publish(<<"Role/Hash">>, <<"SubscriberId">>, 1, <<"TopicName">>, <<"Payload">>, false))
		},
		{"success path 2", setup,
			fun() -> acl_put_(?PUB_KEY, ?ACCOUNT_ID, ?EXP_IN_FUTURE, ?LIMIT, 1, ?IS_RETAIN) end,
			fun(_) -> delete_all_keys(?ACL_BUCKET, [?PUB_KEY]) end,
			?_assertEqual({ok, [{qos, 1}, {retain, false}]},
				vmq_riakacl:auth_on_publish(<<"Role/Hash">>, {[], <<"SubscriberId">>}, 1, [<<"TopicName">>], <<"Payload">>, false))
		},
		{"success path 3", setup,
			fun() -> acl_put_(?PUB_KEY2, ?ACCOUNT_ID, ?EXP_IN_FUTURE, ?LIMIT, 1, ?IS_RETAIN) end,
			fun(_) -> delete_all_keys(?ACL_BUCKET, [?PUB_KEY2]) end,
			?_assertEqual({ok, [{qos, 1}, {retain, false}]},
				vmq_riakacl:auth_on_publish(<<"Role/Hash">>, {[], <<"SubscriberId">>}, 1, [<<"Name1">>, <<"Name2">>], <<"Payload">>, false))
		},
		{"replace qos", setup,
			fun() -> acl_put_(?PUB_KEY, ?ACCOUNT_ID, ?EXP_IN_FUTURE, ?LIMIT, 1, ?IS_RETAIN) end,
			fun(_) -> delete_all_keys(?ACL_BUCKET, [?PUB_KEY]) end,
			?_assertEqual({ok, [{qos, 1}, {retain, false}]},
				vmq_riakacl:auth_on_publish(<<"Role/Hash">>, <<"SubscriberId">>, 2, <<"TopicName">>, <<"Payload">>, false))
		},
		{"acl retain flag is false", setup,
			fun() -> acl_put_(?PUB_KEY, ?ACCOUNT_ID, ?EXP_IN_FUTURE, ?LIMIT, 1, ?IS_RETAIN) end,
			fun(_) -> delete_all_keys(?ACL_BUCKET, [?PUB_KEY]) end,
			?_assertEqual({ok, [{qos, 1}, {retain, false}]},
				vmq_riakacl:auth_on_publish(<<"Role/Hash">>, <<"SubscriberId">>, 1, <<"TopicName">>, <<"Payload">>, true))
		},
		{"acl retain flag is true (request retain is false)", setup,
			fun() -> acl_put_(?PUB_KEY, ?ACCOUNT_ID, ?EXP_IN_FUTURE, ?LIMIT, 1, true) end,
			fun(_) -> delete_all_keys(?ACL_BUCKET, [?PUB_KEY]) end,
			?_assertEqual({ok, [{qos, 1}, {retain, false}]},
				vmq_riakacl:auth_on_publish(<<"Role/Hash">>, <<"SubscriberId">>, 1, <<"TopicName">>, <<"Payload">>, false))
		},
		{"acl retain flag is true (retain true)", setup,
			fun() -> acl_put_(?PUB_KEY, ?ACCOUNT_ID, ?EXP_IN_FUTURE, ?LIMIT, 1, true) end,
			fun(_) -> delete_all_keys(?ACL_BUCKET, [?PUB_KEY]) end,
			?_assertEqual({ok, [{qos, 1}, {retain, true}]},
				vmq_riakacl:auth_on_publish(<<"Role/Hash">>, <<"SubscriberId">>, 1, <<"TopicName">>, <<"Payload">>, true))
		}
	]}.

on_client_offline_test_() ->
	setup(),

	{"on_client_offline", [
		{"sessions exists", setup,
			fun () ->
				session_put_(?ACCOUNT_ID, <<"ClientId">>, broker_id()),
				session_put_(?ACCOUNT_ID, <<"ClientId">>, broker_id()),
				timer:sleep(1000),
				vmq_riakacl:on_client_offline({<<"MountPoint">>, <<"ClientId">>}),
				timer:sleep(1300)
			end,
			fun(_) -> delete_all_keys(?SESSION_BUCKET, bucket_keys(?SESSION_BUCKET)) end,
			?_assertEqual(1,
				length(vmq_riakacl_session:list(#{account_id => <<"Hash">>})))
		},
		{"preserve other clients sessions", setup,
			fun() ->
				session_put_(?ACCOUNT_ID, <<"AnotherClientId">>, broker_id()),
				timer:sleep(1000),
				vmq_riakacl:on_client_offline({<<"MountPoint">>, <<"ClientId">>}),
				timer:sleep(1000)
			end,
			fun(_) -> delete_all_keys(?SESSION_BUCKET, bucket_keys(?SESSION_BUCKET)) end,
			?_assertEqual(1,
				length(vmq_riakacl_session:list(#{account_id => <<"Hash">>})))
		},
		{"no sessions", setup,
			fun() ->
				vmq_riakacl:on_client_offline({<<"MountPoint">>, <<"ClientId">>}),
				timer:sleep(1000)
			end,
			fun(_) -> delete_all_keys(?SESSION_BUCKET, bucket_keys(?SESSION_BUCKET)) end,
			?_assertEqual(0,
				length(vmq_riakacl_session:list(#{account_id => <<"Hash">>})))
		}
	]}.

stop_test_() ->
	setup(),

  {"stop", [
		{"sessions keys cleanup", setup,
			fun () ->
				session_put_(?ACCOUNT_ID, <<"ClientId">>, broker_id()),
				session_put_(?ACCOUNT_ID, <<"ClientId">>, broker_id()),
				timer:sleep(1000),
				vmq_riakacl:stop(),
				timer:sleep(1000),
				vmq_riakacl:start()
			end,
			fun(_) -> delete_all_keys(?SESSION_BUCKET, bucket_keys(?SESSION_BUCKET)) end,
			?_assertEqual(0,
				length(vmq_riakacl_session:list(#{account_id => <<"Hash">>})))
		}
	]}.

-endif.
