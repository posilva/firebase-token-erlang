%% @author <ruel@ruel.me>
%% @copyright 2016 Ruel Pagayon
%% @version 1.1.0
%% @doc Loads service account details from file or from a string.
%% Service account can be retrieved from Google API Management Console
-module(firebase_token_account).

-export([ load/1, load_from_file/1 ]).

-spec load(binary() | string()) -> map() | {error, invalid_account}.
%% @doc Creates a service account instance from JSON string input 
%% @spec load(binary() | string()) -> map() | {error, invalid_account}
load(Content) ->
  JSON         = jsx:decode(Content, [return_maps]),
  Account      = maps:with([<<"client_email">>, <<"private_key">>], JSON),
  validate(Account).

-spec load_from_file(string()) -> map() | {error, invalid_account}.
%% @doc Creates a service account instance from JSON key file
%% @spec load_from_file(string()) -> map() | {error, invalid_account}
load_from_file(Path) ->
  {ok, Binary} = file:read_file(Path),
  JSON         = jsx:decode(Binary, [return_maps]),
  Account      = maps:with([<<"client_email">>, <<"private_key">>], JSON),
  validate(Account).

-spec validate(map()) -> map() | {error, invalid_account}.
%% @private
%% @doc Validates existence of required keys in input map
%% @spec validate(map()) -> map() | {error, invalid_account}
validate(#{ <<"client_email">> := _ClientEmail,
            <<"private_key">>  := _PrivateKey } = Account) ->
  Account;
validate(_Account) ->
  {error, invalid_account}.
