-module(firebase_token_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

init_per_suite(Config) ->
  Config.

end_per_suite(_Config) ->
  ok.

all() ->
  [
    case_wrong_content,
    case_correct_content,
    case_wrong_file,
    case_correct_file,
    case_ok
  ].

case_wrong_content(Config) ->
  WrongFile = filename:join([?config(data_dir, Config), <<"wrong.json">>]), 
  {ok, Content} = file:read_file(WrongFile),
  {error, invalid_account} = firebase_token_account:load(Content),
  ok.

case_correct_content(Config) ->
  File = filename:join([?config(data_dir, Config), <<"test.json">>]), 
  {ok, Content} = file:read_file(File),
  Account = firebase_token_account:load(Content),
  true = maps:is_key(<<"client_email">>, Account),
  true = maps:is_key(<<"private_key">>, Account),
  ok.

case_wrong_file(Config) ->
  WrongFile = filename:join([?config(data_dir, Config), <<"wrong.json">>]), 
  {error, invalid_account} = firebase_token_account:load_from_file(WrongFile),
  ok.

case_correct_file(Config) ->
  File = filename:join([?config(data_dir, Config), <<"test.json">>]), 
  Account = firebase_token_account:load_from_file(File),
  true = maps:is_key(<<"client_email">>, Account),
  true = maps:is_key(<<"private_key">>, Account),
  ok.

case_ok(Config) ->
  File = filename:join([?config(data_dir, Config), <<"test.json">>]), 
  Account = firebase_token_account:load_from_file(File),
  {token, _Token} = firebase_token:generate(Account, "1", 3600, #{}),
  ok.
