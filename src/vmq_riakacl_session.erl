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

-module(vmq_riakacl_session).

%% API
-export([
	bucket/0,
	content_type/0,
	index/0,
	list/1
]).

%% =============================================================================
%% API
%% =============================================================================

-spec bucket() -> tuple().
bucket() ->
	{<<"vmq-riakacl-session_t">>, <<"vmq-riakacl-session">>}.

-spec content_type() -> binary().
content_type() ->
	<<"application/json">>.

-spec list(map()) -> list(binary()).
list(#{account_id := AccountId}) ->
	list_([index(), <<"account_id:", AccountId/binary>>]);
list(#{client_id := ClientId}) ->
	list_([index(), <<"client_id:", ClientId/binary>>]).

-spec index() -> binary().
index() ->
	<<"vmq-riakacl-session_idx">>.

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec list_(list()) -> list(binary()).
list_(Query) ->
	case catch riakc_pool:query(default, search, Query) of
		{ok, {search_results, Docs, _, _}} -> parse_docs(Docs);
		{error, Reason}                    -> exit(Reason);
		{'EXIT', Reason}                   -> exit(Reason);
		Else                               -> exit({bad_return_value, Else})
	end.

-spec parse_docs([{binary(), [tuple()]}]) -> list(binary()).
parse_docs(Docs) ->
	[begin
		{_, Val} = lists:keyfind(<<"_yz_rk">>, 1, Doc),
		Val
	end || {_Index, Doc} <- Docs].

