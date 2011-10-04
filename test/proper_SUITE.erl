%% Copyright (c) 2011, Anthony Ramine <nox@dev-extend.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(proper_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([ex_http_props/1]).


all() ->
  [ex_http_props].

ex_http_props(_Config) ->
	[] = proper:module(ex_http_props, [{on_output, fun proper_on_output/2}]).

proper_on_output(Format, []) when tl(Format) =:= []; Format =:= "~n" ->
  io:format(user, Format, []);
proper_on_output(Format, Data) ->
  io:format(user, Format, Data),
  io:format(Format, Data).
