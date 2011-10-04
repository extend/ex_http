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

-module(ex_http).
-author('Anthony Ramine <nox@dev-extend.eu>').


-type content_type() :: {mime_type(), parameters()}.
-type content_disposition() :: {binary(), parameters()}.
-type mime_type() :: {binary(), binary()}.
-type parameters() :: [{binary(), binary()}].
-type pos() :: non_neg_integer().

-export_type([content_type/0, content_disposition/0,
              mime_type/0, parameters/0, pos/0]).

-export([content_type/1, content_disposition/1]).
-export([token_k/3, lc_token_k/3, lws_k/3]).



-spec content_type(binary()) -> content_type() | badarg.
%% @doc Parse a Content-Type header value.
content_type(Bin) ->
  Cont = fun ({Type, I}) -> content_type_slash(Bin, I, Type) end,
  lc_token_k(Bin, 0, Cont).

-spec content_type_slash(binary(), pos(), binary()) -> content_type() | badarg.
content_type_slash(Bin, I, Type) ->
  Cont = fun (NewI) -> content_type_subtype(Bin, NewI, Type) end,
  char_k(Bin, I, $/, Cont).

-spec content_type_subtype(binary(), pos(), binary()) ->
                            content_type() | badarg.
content_type_subtype(Bin, I, Type) ->
  Cont = fun ({Subtype, NewI}) ->
               content_type_parameters(Bin, NewI, {Type, Subtype}) end,
  lc_token_k(Bin, I, Cont).

-spec content_type_parameters(binary(), pos(), mime_type()) ->
                               content_type() | badarg.
content_type_parameters(Bin, I, MimeType) ->
  parameters_k(Bin, I, fun (Parameters) -> {MimeType, Parameters} end).


-spec content_disposition(binary()) -> content_disposition() | badarg.
%% @doc Parse a Content-Disposition header value.
content_disposition(Bin) ->
  Cont = fun ({Type, I}) ->
               WCont =
                 fun (NewI) ->
                       content_disposition_parameters(Bin, NewI, Type) end,
               lws_k(Bin, I, WCont) end,
  lc_token_k(Bin, 0, Cont).

-spec content_disposition_parameters(binary(), pos(), binary()) ->
                                      content_disposition() | badarg.
content_disposition_parameters(Bin, I, Type) ->
  parameters_k(Bin, I, fun (Parameters) -> {Type, Parameters} end).


-spec token_k(binary(), pos(), fun(({binary(), pos()}) -> T)) -> T | badarg.
%% @doc Extract a token from a binary and pass it to the given continuation,
%%      along with the position of the end of the token. If no token char is
%%      found at the start position, <em>badarg</em> is directly returned.
token_k(Bin, Start, Cont) ->
  token_k(Bin, Start, Cont, Start).

-spec lc_token_k(binary(), pos(), fun(({binary(), pos()}) -> T)) -> T | badarg.
%% @doc Same as {@link token_k/3} except that the token given to the
%%      continuation is passed with all characters lowercase.
lc_token_k(Bin, I, Cont) ->
  NewCont = fun ({Value, NewI}) -> Cont({binary_to_lower(Value), NewI}) end,
  token_k(Bin, I, NewCont).

-spec token_k(binary(), pos(), fun(({binary(), pos()}) -> T), pos()) ->
               T | badarg.
token_k(Bin, Start, Cont, I) ->
  case I < byte_size(Bin) andalso is_token_char(binary:at(Bin, I)) of
    true -> token_k(Bin, Start, Cont, I + 1);
    false ->
      case I of
        Start -> badarg;
        _ -> Cont({binary_part(Bin, Start, I - Start), I}) end end.


-spec lws_k(binary(), pos(), fun((pos()) -> T)) -> T.
%% @doc Skip any linear whitespace at a given position in a binary and call
%%      the continuation with the new position.
lws_k(Bin, I, Cont) ->
  lws_k(Bin, I, Cont, I).

-spec lws_k(binary(), pos(), fun((pos()) -> T), pos()) -> T.
lws_k(Bin, Start, Cont, I) when I =:= byte_size(Bin) ->
  Cont(Start);
lws_k(Bin, Start, Cont, I) ->
  case binary:at(Bin, I) of
    $\s -> lws_k(Bin, I + 1, Cont, I + 1);
    $\t -> lws_k(Bin, I + 1, Cont, I + 1);
    $\r -> lws_k_nl(Bin, I, Cont, I + 1);
    _ -> Cont(Start) end.

-spec lws_k_nl(binary(), pos(), fun((pos()) -> T)) -> T.
lws_k_nl(Bin, I, Cont) ->
  lws_k_nl(Bin, I, Cont, I).

-spec lws_k_nl(binary(), pos(), fun((pos()) -> T), pos()) -> T.
lws_k_nl(Bin, Start, Cont, I) ->
  case binary:at(Bin, I) =:= $\n of
    true -> lws_k_sp(Bin, Start, Cont, I + 1);
    false -> Cont(Start) end.

-spec lws_k_sp(binary(), pos(), fun((pos()) -> T), pos()) -> T.
lws_k_sp(Bin, Start, Cont, I) when I =:= byte_size(Bin) ->
  Cont(Start);
lws_k_sp(Bin, Start, Cont, I) ->
  case binary:at(Bin, I) =:= $\s orelse binary:at(Bin, I) =:= $\t of
    true -> lws_k(Bin, I + 1, Cont, I + 1);
    false -> Cont(Start) end.


-spec parameters_k(binary(), pos(), fun((parameters()) -> T)) -> T | badarg.
parameters_k(Bin, I, Cont) ->
  parameters_k_lws(Bin, I, Cont, []).

-spec parameters_k_lws(binary(), pos(), fun((parameters()) -> T),
                        parameters()) -> T | badarg.
parameters_k_lws(Bin, I, Cont, Acc) ->
  lws_k(Bin, I, fun (NewI) -> parameters_k_sep(Bin, NewI, Cont, Acc) end).

-spec parameters_k_sep(binary(), pos(), fun((parameters()) -> T),
                       parameters()) -> T | badarg.
parameters_k_sep(Bin, I, Cont, Acc) when I =:= byte_size(Bin) ->
  Cont(Acc);
parameters_k_sep(Bin, I, Cont, Acc) ->
  LWSCont = fun (LWSI) -> parameters_k_attr(Bin, LWSI, Cont, Acc) end,
  CharCont = fun (CharI) -> lws_k(Bin, CharI, LWSCont) end,
  char_k(Bin, I, $;, CharCont).

-spec parameters_k_attr(binary(), pos(), fun((parameters()) -> T),
                        parameters()) -> T | badarg.
parameters_k_attr(Bin, I, Cont, Acc) ->
  NewCont = fun ({Arg, NewI}) ->
                  WCont = fun (WI) ->
                                parameters_k_equal(Bin, WI, Cont, Acc, Arg) end,
                  lws_k(Bin, NewI, WCont) end,
  lc_token_k(Bin, I, NewCont).

-spec parameters_k_equal(binary(), pos(), fun((parameters()) -> T),
                         parameters(), binary()) -> T | badarg.
parameters_k_equal(Bin, I, Cont, Acc, Attr) ->
  LWSCont = fun (LWSI) -> parameters_k_value(Bin, LWSI, Cont, Acc, Attr) end,
  CharCont = fun (CharI) -> lws_k(Bin, CharI, LWSCont) end,
  char_k(Bin, I, $=, CharCont).

-spec parameters_k_value(binary(), pos(), fun((parameters()) -> T),
                         parameters(), binary()) -> T | badarg.
parameters_k_value(Bin, I, _Cont, _Acc, _Attr) when I =:= byte_size(Bin) ->
  badarg;
parameters_k_value(Bin, I, Cont, Acc, Attr) ->
  NewCont = fun ({Value, NewI}) ->
                  parameters_k_lws(Bin, NewI, Cont, [{Attr, Value} | Acc]) end,
  case binary:at(Bin, I) of
    $" ->
      quoted_string_k_contents(Bin, I + 1, NewCont);
    _ ->
      token_k(Bin, I, NewCont) end.


-spec quoted_string_k_contents(binary(), pos(),
                               fun(({binary(), pos()}) -> T)) -> T | badarg.
quoted_string_k_contents(Bin, I, Cont) ->
  quoted_string_k_lws(Bin, I, Cont, I, []).

-spec quoted_string_k_lws(binary(), pos(), fun(({binary(), pos()}) -> T), pos(),
                           iodata()) -> T | badarg.
quoted_string_k_lws(Bin, _Start, _Cont, I, _Acc) when I =:= byte_size(Bin) ->
  badarg;
quoted_string_k_lws(Bin, Start, Cont, I, Acc) ->
  case binary:at(Bin, I) of
    $\r ->
      NewCont = fun (NewI) ->
                      NewAcc = [$\s, binary_part(Bin, Start, I - Start) | Acc],
                      quoted_string_k_quoted(Bin, NewI, Cont, NewI, NewAcc) end,
      lws_k_nl(Bin, I + 1, NewCont);
    _ ->
      quoted_string_k_quoted(Bin, Start, Cont, I, Acc) end.

-spec quoted_string_k_quoted(binary(), pos(), fun(({binary(), pos()}) -> T),
                             pos(), iodata()) -> T | badarg.
quoted_string_k_quoted(Bin, _Start, _Cont, I, _Acc) when I =:= byte_size(Bin) ->
  badarg;
quoted_string_k_quoted(Bin, Start, Cont, I, Acc) ->
  case binary:at(Bin, I) of
    $" ->
      IoList = lists:reverse(Acc, binary_part(Bin, Start, I - Start)),
      Cont({iolist_to_binary(IoList), I + 1});
    $\\ ->
      NewI = I + 1,
      case NewI =/= byte_size(Bin) andalso binary:at(Bin, NewI) < 128 of
        true ->
          NewAcc = [binary_part(Bin, Start, I - Start) | Acc],
          quoted_string_k_lws(Bin, NewI, Cont, NewI + 1, NewAcc);
        false ->
          badarg end;
    C when C > 31 andalso C =/= 127 ->
      quoted_string_k_lws(Bin, Start, Cont, I + 1, Acc);
    _ ->
      badarg end.


-spec char_k(binary(), pos(), byte(), fun((pos()) -> T)) -> T | badarg.
char_k(Bin, I, C, Cont) ->
  case I < byte_size(Bin) andalso binary:at(Bin, I) =:= C of
    true -> Cont(I + 1);
    false -> badarg end.


-spec is_token_char(byte()) -> boolean().
%% @doc Return true for any CHAR except CTLs and separators.
is_token_char($() -> false;
is_token_char($)) -> false;
is_token_char($<) -> false;
is_token_char($>) -> false;
is_token_char($@) -> false;
is_token_char($,) -> false;
is_token_char($;) -> false;
is_token_char($:) -> false;
is_token_char($\\) -> false;
is_token_char($") -> false;
is_token_char($/) -> false;
is_token_char($[) -> false;
is_token_char($]) -> false;
is_token_char($?) -> false;
is_token_char($=) -> false;
is_token_char(${) -> false;
is_token_char($}) -> false;
is_token_char(C) when is_integer(C) ->
  C > 32 andalso C < 127.


-spec binary_to_lower(binary()) -> binary().
%% @doc Return a binary where <em>$A..$Z</em> is returned as lowercase
%%      <em>$a..$z</em>. If there is no uppercase characters, the same binary is
%%      returned and nothing is copied.
binary_to_lower(Bin) when is_binary(Bin) ->
  binary_to_lower(Bin, 0).

-spec binary_to_lower(binary(), non_neg_integer()) -> binary().
binary_to_lower(Bin, I) when I =:= byte_size(Bin) ->
  Bin;
binary_to_lower(Bin, I) ->
  C = binary:at(Bin, I),
  case char_to_lower(C) of
    C -> binary_to_lower(Bin, I + 1);
    LowerC ->
      <<Head:I/binary, _, Tail/binary>> = Bin,
      LowerTail = << <<(char_to_lower(C2))>> || <<C2>> <= Tail >>,
      <<Head/binary, LowerC, LowerTail/binary>> end.

-spec char_to_lower(byte()) -> byte().
%% @doc Return uppercase characters as lowercase, the others are returned as is.
char_to_lower($A) -> $a;
char_to_lower($B) -> $b;
char_to_lower($C) -> $c;
char_to_lower($D) -> $d;
char_to_lower($E) -> $e;
char_to_lower($F) -> $f;
char_to_lower($G) -> $g;
char_to_lower($H) -> $h;
char_to_lower($I) -> $i;
char_to_lower($J) -> $j;
char_to_lower($K) -> $k;
char_to_lower($L) -> $l;
char_to_lower($M) -> $m;
char_to_lower($N) -> $n;
char_to_lower($O) -> $o;
char_to_lower($P) -> $p;
char_to_lower($Q) -> $q;
char_to_lower($R) -> $r;
char_to_lower($S) -> $s;
char_to_lower($T) -> $t;
char_to_lower($U) -> $u;
char_to_lower($V) -> $v;
char_to_lower($W) -> $w;
char_to_lower($X) -> $x;
char_to_lower($Y) -> $y;
char_to_lower($Z) -> $z;
char_to_lower(Ch) -> Ch.
