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

-module(ex_multipart).
-author('Anthony Ramine <nox@dev-extend.eu>').

-type part_parser() :: parser(part_result()).
-type parser(T) :: fun((binary()) -> T).
-type part_result() :: more(headers() | eof).
-type more(T) :: T | {more, parser(T)}.
-type headers() :: {headers, http_headers(), body_cont()}.
-type http_headers() :: [{atom() | binary(), binary()}].
-type body_cont() :: cont(body()).
-type cont(T) :: fun(() -> T).
-type body() :: more({body, binary(), body_cont()} | end_of_part()).
-type end_of_part() :: {end_of_part, cont(part_result())}.

-export([parser/1]).


-spec parser(binary()) -> part_parser().
% @doc A multipart parser for the given boundary.
parser(Boundary) when is_binary(Boundary) ->
  fun (Bin) when is_binary(Bin) -> parse(Bin, Boundary) end.


-spec parse(binary(), binary()) -> part_result().
parse(Bin, Boundary) when byte_size(Bin) >= byte_size(Boundary) + 2 ->
  BoundarySize = byte_size(Boundary),
  Pattern = pattern(Boundary),
  case Bin of
    <<"--", Boundary:BoundarySize/binary, _/binary>> ->
      % Data starts with initial boundary, skip preamble parsing.
      parse_boundary_tail(Bin, Pattern, BoundarySize + 2);
    _ ->
      % Parse preamble.
      skip(Bin, Pattern) end;
parse(Bin, Boundary) ->
  % Not enough data to know if the data begins with a boundary.
  more(Bin, fun (NewBin) -> parse(NewBin, Boundary) end).

-spec pattern(binary()) -> pattern().
pattern(Boundary) ->
  MatchPattern = <<"\r\n--", Boundary/binary>>,
  {binary:compile_pattern(MatchPattern), byte_size(MatchPattern)}.

-type pattern() :: {binary:cp(), pos()}.
-type pos() :: non_neg_integer().

-spec parse_boundary_tail(binary(), pattern(), pos()) -> part_result().
parse_boundary_tail(Bin, Pattern, I) when I + 1 < byte_size(Bin) ->
  case binary:at(Bin, I) =:= $- andalso binary:at(Bin, I + 1) =:= $- of
    true ->
      % Boundary is followed by "--", end parsing.
      eof;
    false ->
      % No dash after boundary, proceed with unknown chars and lwsp removal.
      parse_boundary_eol(Bin, Pattern, I) end;
parse_boundary_tail(Bin, Pattern, _I) ->
  % Boundary may be followed by "--", need more data.
  Rest = <<(binary:last(Bin))>>,
  more(Rest, fun (NewBin) -> parse_boundary_tail(NewBin, Pattern, 0) end).

-spec parse_boundary_eol(binary(), pattern(), pos()) -> part_result().
parse_boundary_eol(Bin, Pattern, I) ->
  case binary:match(Bin, <<"\r\n">>, [{scope, {I, byte_size(Bin) - I}}]) of
    {CrlfStart, _Length} ->
      % End of line found, remove optional LWSP
      WCont = fun (NewI) -> parse_boundary_crlf(Bin, Pattern, NewI) end,
      ex_http:lws_k(Bin, CrlfStart, WCont);
    nomatch ->
      % CRLF not found in the given binary from I to the end of it, CRLF is
      % two bytes long so Bin can be truncated to only its last byte.
      Rest = <<(binary:last(Bin))>>,
      Cont = fun (NewBin) -> parse_boundary_eol(NewBin, Pattern, 0) end,
      more(Rest, Cont) end.

-spec parse_boundary_crlf(binary(), pattern(), pos()) -> part_result().
parse_boundary_crlf(Bin, Pattern, I) ->
  % The binary is at least I + 2 bytes long as this function is only called by
  % parse_boundary_eol/3 when CRLF has been found so a more tuple will never
  % be returned from here.
  case binary:at(Bin, I) =:= $\r andalso binary:at(Bin, I + 1) =:= $\n of
    true ->
      RestStart = I + 2,
      <<_:RestStart/binary, Rest/binary>> = Bin,
      parse_headers(Rest, Pattern);
    false ->
      % Unspecified behaviour here: RFC 2046 doesn't say what to do when LWSP is
      % not followed directly by a new line. In this implementation it is
      % considered part of the boundary so EOL needs to be searched again.
      parse_boundary_eol(Bin, Pattern, I) end.

-spec parse_headers(binary(), pattern()) -> part_result().
parse_headers(Bin, Pattern) ->
  parse_headers(Bin, Pattern, []).

-spec parse_headers(binary(), pattern(), http_headers()) -> part_result().
parse_headers(Bin, Pattern, Acc) ->
  case erlang:decode_packet(httph_bin, Bin, []) of
    {ok, {http_header, _, Name, _, Value}, Rest} ->
      parse_headers(Rest, Pattern, [{Name, Value} | Acc]);
    {ok, http_eoh, Rest} ->
      {headers, Acc, fun () -> parse_body(Rest, Pattern) end};
    {ok, {http_error, _}, _} ->
      % Skip malformed parts.
      skip(Bin, Pattern);
    {more, _} ->
      more(Bin, fun (NewBin) -> parse_headers(NewBin, Pattern, Acc) end) end.

-spec parse_body(binary(), pattern()) -> body().
parse_body(Bin, Pattern = {P, PSize}) when byte_size(Bin) >= PSize ->
  case binary:match(Bin, P) of
    {0, _Length} ->
      end_of_part(Bin, Pattern, PSize);
    {BoundaryStart, _Length} ->
      % Boundary found, this is the latest partial body that will be returned
      % for this part.
      <<PartialBody:BoundaryStart/binary, _:PSize/binary, Rest/binary>> = Bin,
      ContResult = end_of_part(Rest, Pattern, 0),
      {body, PartialBody, fun () -> ContResult end};
    nomatch ->
      PartialLength = byte_size(Bin) - PSize + 1,
      <<PartialBody:PartialLength/binary, Rest/binary>> = Bin,
      {body, PartialBody, fun () -> parse_body(Rest, Pattern) end} end;
parse_body(Bin, Pattern) ->
  more(Bin, fun (NewBin) -> parse_body(NewBin, Pattern) end).

-spec end_of_part(binary(), pattern(), pos()) -> end_of_part().
end_of_part(Bin, Pattern, I) ->
  {end_of_part, fun () -> parse_boundary_tail(Bin, Pattern, I) end}.

-spec skip(binary(), pattern()) -> part_result().
skip(Bin, Pattern = {P, PSize}) ->
  BinSize = byte_size(Bin),
  case binary:match(Bin, P) of
    {BoundaryStart, _Length} ->
      % Initial boundary found, skip preamble and proceed with headers parsing
      % of the first part.
      parse_boundary_tail(Bin, Pattern, BoundaryStart + PSize);
    nomatch ->
      % Boundary not found, need more data.
      RestStart = max(byte_size(Bin) - PSize + 1, 0),
      <<_:RestStart/binary, Rest/binary>> = Bin,
      more(Rest, fun (NewBin) -> skip(NewBin, Pattern) end) end.

-spec more(binary(), fun((binary()) -> T)) -> {more, fun((binary()) -> T)}.
more(Bin, F) ->
  {more, fun (NewData) when is_binary(NewData) ->
               F(<<Bin/binary, NewData/binary>>) end}.
