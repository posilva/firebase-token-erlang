%% @author <ruel@ruel.me>
%% @copyright 2016 Ruel Pagayon
%% @version 1.0.0
%% @doc Helper module for generating custom Firebase token in Erlang.
%% Custom Firebase tokens are used on applications with totally different
%% authentication methods. These tokens are in JWT (JSON Web Token) format
%% and can be then used by client applications on authenticating to Firebase.
%% 
%% Only supports Firebase 3.x.x.
%% See https://firebase.google.com/docs/auth/server/create-custom-tokens
%% for more information.
-module(firebase_token).

-define(TOKEN_VERSION, 0).

-export([ load_account/1, generate/4 ]).

-spec load_account(string()) -> map() | {error, invalid_account}.
%% @doc Creates a service_account() from JSON key file or url
%% @spec load_account(string()) -> map() | {error, invalid_account}
load_account(Path) ->
  {ok, Binary} = file:read_file(Path),
  JSON         = jsx:decode(Binary, [return_maps]),
  Account      = maps:with([<<"client_email">>, <<"private_key">>], JSON),
  validate_account(Account).

-spec generate(map(), binary(), integer(), map()) -> 
   {token, binary()} | {error, invalid_account}.
%% @doc Generates the custom Firebase token.
%% @spec generate(map(), binary(), integer(), map()) -> 
%%   {token, binary()} | {error, invalid_account}
generate(#{ <<"client_email">> := ClientEmail, 
            <<"private_key">>  := PrivateKey } = _Account, Uid, Life, Extra) ->
  Claims        = build_claims(Uid, ClientEmail, Life, Extra),
  JWK           = jose_jwk:from_pem(PrivateKey),
  {_JWS, Token} = sign(JWK, Claims),
  {token, Token};
generate(_Account, _Uid, _Life, _Extra) ->
  {error, invalid_account}.

-spec validate_account(map()) -> map() | {error, invalid_account}.
%% @private
%% @doc Validates existence of required keys in input map
%% @spec validate_account(map()) -> map() | {error, invalid_account}
validate_account(#{ <<"client_email">> := _ClientEmail,
                    <<"private_key">>  := _PrivateKey } = Account) ->
  Account;
validate_account(_Account) ->
  {error, invalid_account}.

-type uid() :: binary() | string().
-spec build_claims(uid(), binary(), integer(), map()) -> map().
%% @private
%% @doc Builds the payload with the standard keys
%% and cleans the options with the optional keys
%% @spec build_claims(uid(), binary(), integer(), map()) -> map()
build_claims(Uid, ClientEmail, Life, Extra) ->
  Now = erlang:system_time(seconds),
  #{
    <<"iss">>    => ClientEmail,
    <<"sub">>    => ClientEmail,
    <<"aud">>    => <<"https://identitytoolkit.googleapis.com/google.identity.identitytoolkit.v1.IdentityToolkit">>,
    <<"uid">>    => Uid,
    <<"iat">>    => Now,
    <<"exp">>    => Now + Life,
    <<"claims">> => Extra 
  }.

-type jwk() :: map().
-spec sign(jwk(), map()) -> binary().
%% @private
%% @doc Signs the claims and generates the JWT token
%% Then returns it in url-safe base64 format.
%% @spec sign(jwk(), map()) -> binary()
sign(JWK, Claims) ->
  JWS =  #{
    <<"alg">> => <<"RS256">>
  },
  Signed = jose_jwt:sign(JWK, JWS, Claims),
  jose_jws:compact(Signed).
