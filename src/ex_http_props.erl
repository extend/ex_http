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

-module(ex_http_props).
-author('Anthony Ramine <nox@dev-extend.eu>').

-include_lib("proper/include/proper.hrl").

-export([prop_content_type_correctly_parsed/0,
         prop_content_disposition_correctly_parsed/0]).

-spec prop_content_type_correctly_parsed() -> term().
%% @doc Whether content types are correctly parsed.
%% @see ex_http:content_type/1.
prop_content_type_correctly_parsed() ->
  ?FORALL(Expected, ex_http_types:content_type(),
  ?FORALL(R, ex_http_types:content_type_r(Expected),
          is_content_type(ex_http:content_type(R), Expected))).

-spec is_content_type(term(), ex_http:content_type()) -> term().
%% @doc Whether a term is a given content type.
is_content_type({{AType, ASubtype}, AParams}, {{EType, ESubtype}, EParams}) ->
  conjunction([{type, equals(<<"Type">>, AType, EType)},
               {subtype, equals(<<"Subtype">>, ASubtype, ESubtype)},
               {parameters, is_parameters(AParams, EParams)}]);
is_content_type(Actual, _) ->
  ?WHENFAIL(io:format("Result =:= ~w~n", [Actual]), false).


-spec prop_content_disposition_correctly_parsed() -> term().
%% @doc Whether content dispositions are correctly parsed.
%% @see ex_http:content_disposition/1.
prop_content_disposition_correctly_parsed() ->
  ?FORALL(Expected, ex_http_types:content_disposition(),
  ?FORALL(R, ex_http_types:content_disposition_r(Expected),
          is_content_disposition(ex_http:content_disposition(R), Expected))).

-spec is_content_disposition(term(), ex_http:content_disposition()) -> term().
%% @doc Whether a term is a given content type.
is_content_disposition({AType, AParams}, {EType, EParams}) ->
  conjunction([{type, equals(<<"Type">>, AType, EType)},
               {parameters, is_parameters(AParams, EParams)}]);
is_content_disposition(Actual, _) ->
  ?WHENFAIL(io:format("Result =:= ~w~n", [Actual]), false).


-spec equals(binary(), term(), term()) -> term().
%% @doc Whether two given terms are equal.
equals(Prefix, Actual, Expected) ->
  ?WHENFAIL(io:format("~s ~w =/= ~w", [Prefix, Actual, Expected]),
            Actual =:= Expected).


-spec is_parameters(term(), ex_http:parameters()) -> term().
%% @doc Whether a term is a given list of parameters.
is_parameters(AParams, EParams) ->
  case is_parameters(AParams) of
    true ->
      NAParams = normalize_parameters(AParams),
      NEParams = normalize_parameters(EParams),
      ?WHENFAIL(io:format("Normalized params ~p ~~/= ~p~n",
                          [NAParams, NEParams]),
                NAParams =:= NEParams);
    false ->
      ?WHENFAIL(io:format("Params =:= ~w~n", [AParams]), false) end.

-spec is_parameters(term()) -> boolean().
%% @doc Whether a given term is a list of parameters.
is_parameters([{Attr, Value} | Rest]) when is_binary(Attr), is_binary(Value) ->
  lists:all(fun (C) -> C < $A orelse C > $Z end, binary_to_list(Attr)) andalso
  is_parameters(Rest);
is_parameters([]) ->
  true;
is_parameters(_) ->
  false.


-spec normalize_parameters(ex_http:parameters()) -> ex_http:parameters().
%% @doc Normalize a list of parameters. The attributes are returned lowercase
%%      and all spaces and tabs in values are coalesced to one single space.
%%      The list is then returned sorted.
normalize_parameters(Params) ->
  lists:sort([ normalize_parameter(Param) || Param <- Params ]).

-spec normalize_parameter({binary(), binary()}) -> {binary(), binary()}.
%% @doc Normalize a single parameter.
normalize_parameter({Attr, Value}) ->
  {Attr, re:replace(Value, "[ \t]+", " ", [global, {return, binary}])}.
