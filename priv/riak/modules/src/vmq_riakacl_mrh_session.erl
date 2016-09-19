-module(vmq_riakacl_mrh_session).

%% Map Tasks
-export([
	key/3
]).

%% Reduce Tasks
-export([
	remove/2
]).

key(Obj, _KeyData, _Opts) ->
	[riak_object:key(Obj)].

remove(L, _Opts) ->
	Client = riak_client:new(node(), undefined),
	[riak_client:delete({<<"vmq-riakacl-session_t">>, <<"vmq-riakacl-session">>}, Key, Client) || Key <- L],
	[].
