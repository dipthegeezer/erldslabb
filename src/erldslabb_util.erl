%% =====================================================================
%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% $Id$
%%
%% @copyright 2012 Dipesh Patel
%% @author Dipesh Patel<dipthegeezer.opensource@gmail.com>
%% @end
%% =====================================================================

%% @doc Bunch of utlilty functions.
%%
%% I couldn't decide where to put them so for now they go here.

-module(erldslabb_util).

-export([
         get_timestamp/0,
         hash_password/2
        ]).

%% @spec () -> integer()
%% @doc Return the current timestamp via erlang:now().
get_timestamp() ->
    {Megaseconds,Seconds,Microseconds} = erlang:now(),
    (Megaseconds*1000000+Seconds)*1000000+Microseconds.

%% @spec (iolist() | binary(), iolist() | binary()) -> binary()
%% @doc Hash a Password using hmac and the provided Salt.
hash_password(Salt, Password) ->
    Cxt = crypto:hmac_init(sha,Salt),
    Cxt2 = crypto:hmac_update(Cxt,Password),
    Mac = crypto:hmac_final(Cxt2),
    base64:encode(Mac).

%% @private
%% @doc not currently used will yank at some point
hexstring(<<X:128/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~32.16.0b", [X]));
hexstring(<<X:160/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~40.16.0b", [X]));
hexstring(<<X:256/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~64.16.0b", [X]));
hexstring(<<X:512/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~128.16.0b", [X])).

%%
%% Tests for unexported functions
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

hexstring_test() ->
    Bin128 = <<123456789007845:128/big-unsigned-integer>>,
    ?assertEqual(
       "000000000000000000007048860dcde5",
       hexstring(Bin128)
      ),
    Bin160 = <<123456789007845:160/big-unsigned-integer>>,
    ?assertEqual(
       "00000000000000000000000000007048860dcde5",
       hexstring(Bin160)
      ),
    Bin256 = <<123456789007845:256/big-unsigned-integer>>,
    ?assertEqual(
       "00000000000000000000000000000000000000000000000000007048860dcde5",
       hexstring(Bin256)
      ),
    Bin512 = <<123456789007845:512/big-unsigned-integer>>,
    ?assertEqual(
       "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000007048860dcde5",
       hexstring(Bin512)
      ).

get_timestamp_test_() ->
    {"We get a valid timestamp..",
     fun() ->
         {Megaseconds,_,_} = erlang:now(),
         ?assertMatch( X when X > Megaseconds*1000000000000,
                              erldslabb_util:get_timestamp())
     end
	}.

hash_password_test_() ->
    {"We get the correct hashed password.",
     fun() ->
         HashString = erldslabb_util:hash_password("foo","bar"),
         ?assertEqual(<<"RrTsWGEXFU2s1J1mTl1j/ciO+1E=">>,
                      HashString)
     end
	}.

-endif.
