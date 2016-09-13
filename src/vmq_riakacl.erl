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

%% API
-export([
	read_configs/0,
	read_configs/1,
	read_config/2
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
	config_files/0
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

%% =============================================================================
%% Hooks
%% =============================================================================

auth_on_register(_Peer, _SubscriberId, _UserName, _Password, _CleanSession) ->
	ok.

auth_on_subscribe(_UserName, _SubscriberId, _Topics) ->
	ok.

auth_on_publish(_UserName, _SubscriberId, _QoS, _Topic, _Payload, _IsRetain) ->
	ok.

on_client_offline(_SubscriberId) ->
	ok.

on_client_gone(_SubscriberId) ->
	ok.

%% =============================================================================
%% Plugin Callbacks
%% =============================================================================

-spec start() -> ok.
start() ->
	read_configs(),
	{ok, _} = application:ensure_all_started(?APP),
	ok.

-spec stop() -> ok.
stop() ->
	application:stop(?APP).

%% =============================================================================
%% Configuration
%% =============================================================================

-spec config_files() -> [{atom(), binary()}].
config_files() ->
	Default = [{?APP, "/etc/vernemq/riakacl.conf"}],
	[{App, list_to_binary(Val)}
		|| {App, Val} <- application:get_env(?APP, ?FUNCTION_NAME, Default)].

%% =============================================================================
%% Internal functions 
%% =============================================================================

%% -spec broker_id() -> binary().
%% broker_id() ->
%% 	atom_to_binary(node(), utf8).
%% 
%% -spec priv_path(binary()) -> binary().
%% priv_path(Path) ->
%% 	Priv =
%% 		case code:priv_dir(?APP) of
%% 			{error, _} -> "priv";
%% 			Dir        -> Dir
%% 		end,
%% 	<<(list_to_binary(Priv))/binary, $/, Path/binary>>.

