%% @author <ruel@ruel.me>
%% @copyright 2016 Ruel Pagayon
%% @version 1.1.0
%% @doc Helper module for generating custom Firebase token in Erlang.
%% Custom Firebase tokens are used on applications with totally different
%% authentication methods. These tokens are in JWT (JSON Web Token) format
%% and can be then used by client applications on authenticating to Firebase.
%% 
%% Only supports Firebase 3.x.x.
%% See https://firebase.google.com/docs/auth/server/create-custom-tokens
%% for more information.
-module(firebase_token).

-export([ generate/4 ]).

-type uid() :: binary() | string() | integer().

-spec generate(map(), uid(), integer(), map()) -> {token, binary()}.
%% @doc Generates the custom Firebase token.
%% @spec generate(map(), uid(), integer(), map()) -> {token, binary()}
generate(Account, Uid, Life, #{} = Extra) ->
  #{<<"client_email">> := ClientEmail,
    <<"private_key">>  := PrivateKey } = Account,
  Claims = build_claims(Uid, ClientEmail, Life, Extra),
  JWK    = jose_jwk:from_pem(PrivateKey),
  {_JWS, Token} = sign(JWK, Claims),
  {token, Token}.


-spec build_claims(uid(), binary(), integer(), map()) -> map().
%% @private
%% @doc Builds the payload with the standard keys
%% and cleans the options with the optional keys
%% @spec build_claims(uid(), binary(), integer(), map()) -> map()
build_claims(Uid, ClientEmail, Life, Extra) when Life < 0 ->
  build_claims(Uid, ClientEmail, 0, Extra);
build_claims(Uid, ClientEmail, Life, Extra) when Life > 3600 ->
  build_claims(Uid, ClientEmail, 3600, Extra);
build_claims(Uid, ClientEmail, Life, Extra) when is_integer(Uid) ->
  build_claims(integer_to_binary(Uid), ClientEmail, Life, Extra);
build_claims(Uid, ClientEmail, Life, Extra) when is_list(Uid) ->
  build_claims(list_to_binary(Uid), ClientEmail, Life, Extra);
build_claims(Uid, ClientEmail, Life, Extra) when is_binary(Uid) andalso 
                                                 size(Uid) > 0  andalso 
                                                 size(Uid) < 37 ->
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
