%% ----------------------------------------------------------------------------
%% The MIT License
%%
%% Copyright (c) 2016 Davydenkov Mihail <davydenkov.mihail@gmail.com>
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

-module(vmq_riakacl_acl).

-include_lib("riakc/include/riakc.hrl").

%% API
-export([
	bucket/0,
	con_key/1,
	content_type/0,
	get/2,
	get/3,
	pub_key/2,
	sub_key/2
]).

%% =============================================================================
%% API
%% =============================================================================

-spec bucket() -> bucket_and_type().
bucket() ->
	{<<"vmq-riakacl-acl_t">>, <<"vmq-riakacl-acl">>}.

-spec content_type() -> binary().
content_type() ->
	<<"application/json">>.

-spec get(con, binary()) -> binary().
get(con, AccountId) ->
	get_(con_key(AccountId)).

-spec get(pub | sub, binary(), binary()) -> binary().
get(pub, AccountId, Topic) ->
	get_(pub_key(AccountId, Topic));
get(sub, AccountId, Topic) ->
	get_(sub_key(AccountId, Topic)).

-spec con_key(binary()) -> binary().
con_key(AccountId) ->
	<<AccountId/binary, $:, "con">>.

-spec pub_key(binary(), binary()) -> binary().
pub_key(AccountId, Topic) ->
	NewTopic = topic_to_binary_(Topic),
	<<AccountId/binary, $:, "pub", $:, NewTopic/binary>>.

-spec sub_key(binary(), binary()) -> binary().
sub_key(AccountId, Topic) ->
	NewTopic = topic_to_binary_(Topic),
	<<AccountId/binary, $:, "sub", $:, NewTopic/binary>>.

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec get_(binary()) -> binary().
get_(Key) ->
	case catch riakc_pool:query(default, get, [bucket(), Key]) of
		{ok, Result}      -> Result;
		{error, notfound} -> error({bad_key, Key});
		{error, Reason}   -> exit(Reason);
		{'EXIT', Reason}  -> exit(Reason);
		Else              -> exit({bad_return_value, Else})
	end.

-spec topic_to_binary_(binary() | [binary()]) -> binary().
topic_to_binary_(Topic) when is_binary(Topic) ->
	Topic;
topic_to_binary_([]) -> <<>>;
topic_to_binary_([H|T]) -> topic_to_binary_(T, <<H/binary>>).

topic_to_binary_([], Acc) -> Acc;
topic_to_binary_([H|T], Acc) -> topic_to_binary_(T, <<Acc/binary, $/, H/binary>>).
