-module(erldslabb_util).

-export([
         get_timestamp/0,
         hash_password/2
        ]).


get_timestamp() ->
    {Megaseconds,Seconds,Microseconds} = erlang:now(),
    (Megaseconds*1000000+Seconds)*1000000+Microseconds.

hash_password(Salt, Password) ->
    Cxt = crypto:hmac_init(sha,Salt),
    Cxt2 = crypto:hmac_update(Cxt,Password),
    Mac = crypto:hmac_final(Cxt2),
    hexstring(Mac).

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
         ?assertEqual("46b4ec586117154dacd49d664e5d63fdc88efb51",
                      HashString)
     end
	}.

-endif.
