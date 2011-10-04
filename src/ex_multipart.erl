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

-type http_headers() :: [{binary() | atom(), binary()}].
-type part() :: {http_headers(), binary()}.
-type parser() :: fun((binary()) -> parse_result()).
-type parse_result() :: {data, part(), binary(), parser()}
                      | {more, parser()}
                      | eof.

-export_type([http_headers/0, part/0, parser/0, parse_result/0]).

-export([parser/1]).

-type pattern() :: {binary:cp(), non_neg_integer()}.
-type part_fun() :: fun((binary(), pattern(),
                         non_neg_integer(), binary()) ->
                          parse_result()).


-spec parser(binary()) -> parser().
parser(Boundary) when is_binary(Boundary) ->
  fun (Bin) when is_binary(Bin) -> part(Bin, Boundary) end.


-spec part(binary(), Boundary::binary()) -> parse_result().
part(Bin, Boundary) when byte_size(Bin) >= byte_size(Boundary) + 2 ->
  BoundarySize = byte_size(Boundary),
  case Bin of
    <<"--", Boundary:BoundarySize/binary, _/binary>> ->
      % Data starts with initial boundary, skip preamble parsing.
      part_boundary_tail(Bin, pattern(Boundary), BoundarySize + 2);
    _ ->
      % Parse preamble.
      part_boundary(Bin, pattern(Boundary), fun part_preamble/4, 0) end;
part(Bin, Boundary) ->
  % Not enough data to know if the data begins with a boundary.
  {more, fun (NewData) when is_binary(NewData) ->
               part(<<Bin/binary, NewData/binary>>, Boundary) end}.

-spec part_boundary_tail(binary(), pattern(), non_neg_integer()) ->
                          parse_result().
part_boundary_tail(Bin, Pattern, I) when I >= byte_size(Bin) + 2 ->
  % Boundary may be followed by "--", need more data.
  {more, fun (NewData) when is_binary(NewData) ->
               NewBin = <<Bin/binary, NewData/binary>>,
               part_boundary_tail(NewBin, Pattern, I) end};
part_boundary_tail(Bin, Pattern, I) ->
  case binary:at(Bin, I) =:= $- andalso binary:at(Bin, I + 1) =:= $- of
    true ->
      % Boundary is followed by "--", end parsing.
      eof;
    false ->
      % No dash after boundary, proceed with unknown chars and lwsp removal.
      part_boundary_eol(Bin, Pattern, I) end.

-spec part_boundary_eol(binary(), pattern(), non_neg_integer()) ->
                         parse_result().
part_boundary_eol(Bin, Pattern, I) ->
  case binary:match(Bin, <<"\r\n">>, [{scope, {I, byte_size(Bin) - I}}]) of
    {CrlfStart, _Length} ->
      % End of line found, remove LWSP
      WCont = fun (NewI) -> part_boundary_crlf(Bin, Pattern, NewI) end,
      ex_http:lws_k(Bin, CrlfStart, WCont);
    nomatch ->
      NewI = byte_size(Bin) - 1,
      % No end of line found, still in boundary, more data needed.
      {more, fun (NewData) when is_binary(NewData) ->
                   NewBin = <<Bin/binary, NewData/binary>>,
                   part_boundary_eol(NewBin, Pattern, NewI) end} end.

-spec part_boundary_crlf(binary(), pattern(), non_neg_integer()) ->
                          parse_result().
part_boundary_crlf(Bin, Pattern, I) when I + 1 >= byte_size(Bin) ->
  {more, fun (NewData) when is_binary(NewData) ->
               NewBin = <<Bin/binary, NewData/binary>>,
               part_boundary_crlf(NewBin, Pattern, I) end};
part_boundary_crlf(Bin, Pattern, I) ->
  case binary:at(Bin, I) =:= $\r andalso binary:at(Bin, I + 1) =:= $\n of
    true ->
      % CRLF found after boundary.
      part_boundary(Bin, Pattern, fun part_contents/4, I + 2);
    false ->
      % Unspecified behaviour here: RFC 2046 doesn't say what to do when LWSP is
      % not followed directly by a new line. In this implementation it is
      % considered part of the boundary so EOL needs to be searched again.
      part_boundary_eol(Bin, Pattern, I) end.

-spec part_boundary(binary(), pattern(), part_fun(), non_neg_integer()) ->
                     parse_result().
part_boundary(Bin, Pattern, PartFun, Start) ->
  part_boundary(Bin, Pattern, PartFun, Start, Start).

-spec part_boundary(binary(), pattern(), part_fun(),
                    non_neg_integer(), non_neg_integer()) ->
                     parse_result().
part_boundary(Bin, Pattern = {P, PSize}, PartFun, Start, I) ->
  % Try to find boundary in the current data.
  case binary:match(Bin, P, [{scope, {I, byte_size(Bin) - I}}]) of
    {BoundaryStart, _Length} ->
      % Boundary found, proceed with part parsing.
      Part = binary_part(Bin, Start, BoundaryStart - Start),
      PartFun(Bin, Pattern, BoundaryStart + PSize, Part);
    nomatch ->
      % Boundary not found, need more data.
      NewI = byte_size(Bin) - PSize + 1,
      More = fun (NewData) when is_binary(NewData) ->
                   NewBin = <<Bin/binary, NewData/binary>>,
                   part_boundary(NewBin, Pattern, PartFun, Start, NewI) end,
      {more, More} end.

-spec part_preamble(binary(), pattern(), non_neg_integer(), binary()) ->
                     parse_result().
part_preamble(Bin, Pattern, I, _Preamble) ->
  % Preamble is just thrown away.
  part_boundary_tail(Bin, Pattern, I).

-spec part_contents(binary(), pattern(), non_neg_integer(), binary()) ->
                     parse_result().
part_contents(Bin, Pattern, I, PartBin) ->
  case decode_part(PartBin) of
    badarg ->
      part_boundary_tail(Bin, Pattern, I);
    Part ->
      Rest = binary_part(Bin, I, byte_size(Bin) - I),
      Parser = fun (NewBin) when is_binary(NewBin) ->
                     part_boundary_tail(NewBin, Pattern, 0) end,
      {data, Part, Rest, Parser} end.


-spec pattern(binary()) -> pattern().
pattern(Boundary) ->
  MatchPattern = <<"\r\n--", Boundary/binary>>,
  {binary:compile_pattern(MatchPattern), byte_size(MatchPattern)}.


-spec decode_part(binary()) -> part() | badarg.
decode_part(Bin) ->
  decode_part(Bin, []).

-spec decode_part(binary(), http_headers()) -> part() | badarg.
decode_part(Bin, Acc) ->
  case erlang:decode_packet(httph_bin, Bin, []) of
    {ok, {http_header, _, Name, _, Value}, Rest} ->
      decode_part(Rest, [{Name, Value} | Acc]);
    {ok, http_eoh, Body} ->
      {Acc, Body};
    _ErrorOrMore ->
      % Malformed and incomplete parts are not valid.
      badarg end.
