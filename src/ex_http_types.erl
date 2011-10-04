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

-module(ex_http_types).
-author('Anthony Ramine <nox@dev-extend.eu>').

-include_lib("proper/include/proper.hrl").

-export([content_type/0, content_type_r/1]).
-export([content_disposition/0, content_disposition_r/1]).
-export([parameters/0, parameters_r/1]).


-spec content_type() -> {{proper_types:type(), proper_types:type()},
                         proper_types:type()}.
%% @doc All content types.
%% @see ex_http:content_type().
content_type() ->
  {{lc_token(), lc_token()}, parameters()}.

-spec content_type_r(ex_http:content_type()) -> proper_types:type().
%% @doc All representations of a given content type.
content_type_r({{Type, Subtype}, Parameters}) ->
  MimeTypeR = to_binary([lc_token_r(Type), $/, lc_token_r(Subtype)]),
  case Parameters of
    [] -> MimeTypeR;
    _ -> optional_lws_join([MimeTypeR, parameters_r(Parameters)]) end.


-spec content_disposition() -> {proper_types:type(), proper_types:type()}.
%% @doc All content dispositions.
%% @see ex_http:content_disposition().
content_disposition() ->
  {lc_token(), parameters()}.

-spec content_disposition_r(ex_http:content_disposition()) ->
                             proper_types:type().
%% @doc All representations of a given content disposition.
content_disposition_r({Type, Parameters}) ->
  TypeR = lc_token_r(Type),
  case Parameters of
    [] -> TypeR;
    _ -> optional_lws_join([TypeR, parameters_r(Parameters)]) end.


-spec parameters() -> proper_types:type().
%% @doc All parameter lists.
%% @see ex_http:parameters().
parameters() ->
  list(parameter()).

-spec parameters_r(ex_http:parameters()) -> proper_types:type().
%% @doc All representations of a given list of parameters.
parameters_r(Params) ->
  prefix_join([ parameter_r(Param) || Param <- Params ], $;).

-spec parameter() -> {proper_types:type(), proper_types:type()}.
%% @doc All parameters.
parameter() ->
  {lc_token(), parameter_value()}.

-spec parameter_r({binary(), binary()}) -> proper_types:type().
%% @doc All representations of a given parameter.
parameter_r({Attribute, Value}) ->
  separator_join([lc_token_r(Attribute), parameter_value_r(Value)], $=).

-spec parameter_value() -> proper_types:type().
%% @doc All parameter values.
parameter_value() ->
  wunion([{10, token()}, {1, quoted_string()}]).

-spec parameter_value_r(binary()) -> proper_types:type().
%% @doc All representations of a given parameter value.
parameter_value_r(Value) ->
  case is_token(Value) of
    true -> union([Value, quoted_string_r(Value)]);
    false -> quoted_string_r(Value) end.


-spec token() -> proper_types:type().
%% @doc All tokens.
token() ->
  to_binary(token_chars()).

-spec lc_token() -> proper_types:type().
%% @doc All lowercase tokens.
lc_token() ->
  ?LET(Chars, token_chars(), to_binary(string:to_lower(Chars))).

-spec lc_token_r(binary()) -> proper_types:type().
%% @doc All representations of a given token.
lc_token_r(Token) ->
  to_binary([ union([C, string:to_upper([C])]) || <<C>> <= Token ]).

-spec token_chars() -> proper_types:type().
%% @doc All non empty lists of token characters.
token_chars() ->
  non_empty(list(token_char())).

-spec token_char() -> proper_types:type().
%% @doc All token characters.
%%      An US ASCII character that is not a control character or a separator.
token_char() ->
  union([choose($a, $z), ?SUCHTHAT(C, choose(0, 127), is_token_char(C))]).

-spec is_token(binary()) -> boolean().
%% @doc Whether a given binary is a token.
is_token(<<>>) ->
  false;
is_token(Bin) ->
  lists:all(fun is_token_char/1, binary_to_list(Bin)).

-spec is_token_char(byte()) -> boolean().
%% @doc Whether a given char is allowed in a token.
is_token_char(C) ->
  C > 31 andalso C < 127 andalso not lists:member(C, separators()).

-spec separators() -> [byte(), ...].
%% @doc All separators as defined by RFC 2616.
separators() ->
  [$(, $), $<, $>, $@, $,, $;, $:, $\\, $",
   $/, $[, $], $?, $=, ${, $}, $\s, $\t].


-spec quoted_string() -> proper_types:type().
%% @doc All quoted strings.
quoted_string() ->
  Char = wunion([{10, ?SUCHTHAT(C1, byte(), C1 > 31 andalso C1 =/= 127)},
                 {5, union([$", $\\,  
                            ?SUCHTHAT(C2, choose(0, 127),
                                      C2 =/= $\r andalso C2 =/= $\n)])}]),
  to_binary(list(Char)).

-spec quoted_string_r(binary()) -> proper_types:type().
%% @doc All representations of a given quoted string.
quoted_string_r(QuotedString) ->
  to_binary([$", quoted_string_r_contents(QuotedString), $"]).

-spec quoted_string_r_contents(binary()) -> proper_types:type().
%% @doc All representations of a given quoted string contents. Any space
%%      character may be replaced by linear whitespace, all double quotes and
%%      backslashes are escaped and any char may be escaped.
quoted_string_r_contents(Bin) ->
  ?LET(Bin2, with_lws(Bin), with_escaped_chars(Bin2)).

-spec with_lws(binary()) -> proper_types:type().
%% @doc All representations of a given binary where space characters
%%      have the same semantics as mandatory linear whitespace.
with_lws(Bin) ->
  to_binary(with_lws_2(binary_to_list(Bin))).
  
-spec with_lws_2([byte()]) -> [proper_types:type() | byte()].
%% @doc Same as with_lws/1 but with a list of bytes.
with_lws_2([C | Rest]) when C =:= $\s; C =:= $\t ->
  [wunion([{2, union([$\s, $\t])}, {1, mandatory_lws()}]) | with_lws_2(Rest)];
with_lws_2([C | Rest]) ->
  [C | with_lws_2(Rest)];
with_lws_2([]) ->
  [].

-spec with_escaped_chars(binary()) -> proper_types:type().
%% @doc All representations of a given binary where all double quotes,
%%      backslashes and control characters (except CRLF) have been escaped,
%%      other US ASCII characters (except CRLF and the char following it) may
%%      also be escaped.
with_escaped_chars(Bin) ->
  to_binary(with_escaped_chars_2(binary_to_list(Bin))).

-spec with_escaped_chars_2([byte()]) -> [proper_types:type() | byte()].
%% @doc Same as with_escaped_chars/1 but with a list of bytes.
with_escaped_chars_2([$\r, $\n, C | Rest]) ->
  [$\r, $\n, C | with_escaped_chars_2(Rest)];
with_escaped_chars_2([C | Rest]) when C =< 31, C =/= $\r, C =/= $\n;
                                      C =:= $"; C =:= $\\; C =:= 127 ->
  [$\\, C | with_escaped_chars_2(Rest)];
with_escaped_chars_2([C | Rest]) when C < 128 ->
  [wunion([{10, C}, {1, [$\\, C]}]) | with_escaped_chars_2(Rest)];
with_escaped_chars_2([C | Rest]) ->
  [C | with_escaped_chars_2(Rest)];
with_escaped_chars_2([]) ->
  [].


-spec mandatory_lws() -> proper_types:type().
%% @doc All mandatory linear whitespace.
mandatory_lws() ->
  non_empty(optional_lws()).

-spec optional_lws() -> proper_types:type().
%% @doc All optional linear whitespace (may be an empty binary).
optional_lws() ->
  to_binary(list([union([[], [$\r, $\n]]), union([$\s, $\t])])).


-spec prefix_join([term(), ...], term()) -> proper_types:type().
%% @doc Take a list of <em>iodata()</em> types and return them as a
%%      <em>binary()</em> type representing their concatenation where each of
%%      them is prefixed by another type and optional linear whitespace.
prefix_join(List = [_ | _], Sep) ->
  ?LET(Tail, separator_join(List, Sep), optional_lws_join([Sep, Tail])).

-spec separator_join([term(), ...], term()) -> proper_types:type().
%% @doc Take a list of <em>iodata()</em> types and return them as a
%%      <em>binary()</em> type representing their concatenation with optional
%%      linear whitespace and a mandatory separator between each of them.
separator_join([Head | Tail], Separator) ->
  optional_lws_join([Head | lists:append([ [Separator, X] || X <- Tail ])]).

-spec optional_lws_join([term(), ...]) -> proper_types:type().
%% @doc Take a non empty list of <em>iodata()</em> types and return them as a
%%      <em>binary()</em> type representing their concatenation with optional
%%      linear whitespace between each of them.
optional_lws_join([Head | Tail]) ->
  to_binary([Head | [ ?LET(LWS, [optional_lws()], [LWS, X]) || X <- Tail ]]).


-spec to_binary(term()) -> proper_types:type().
%% @doc Take an <em>iodata()</em> type and return it as a <em>binary()</em> one.
to_binary(Type) ->
  ?LET(Value, Type, iolist_to_binary(Value)).
