# Firebase Token Generator - Erlang 

[![Build Status](https://travis-ci.org/ruel/firebase-token-erlang.svg?branch=master)](https://travis-ci.org/ruel/firebase-token-erlang) [![Hex.pm](https://img.shields.io/hexpm/v/firebase_token.svg)](https://hex.pm/packages/firebase_token)

Helper module for generating custom Firebase token in Erlang. Custom Firebase tokens are used on applications with totally different authentication methods. These tokens are in JWT (JSON Web Token) format and can be then used by client applications on authenticating to Firebase.

Only supports Firebase 3.x.x. See https://firebase.google.com/docs/auth/server/create-custom-tokens for more information.

## OTP Version

**Required**: OTP 18 and later

## Setup

This can be added as a dependency from [hex.pm](https://hex.pm/packages/firebase_token)

```
{deps, [
  {firebase_token, "1.0.0"}
]}. 
```

## Usage

One of the requirements for this library is the service account key JSON file that can be obtained from the [Google API Manager Console](https://console.developers.google.com/apis/credentials)

```erlang
Account = firebase_token:load_account("/path/to/service_account.json"),
Uid = <<"1">>, %% Main user id string. Length must not exceed by 36
Life = 3600,   %% Token life (when to expire). Must not exceed 3600 seconds
Extra = #{
  %% Arbitrary values
  admin => true
},

{token, _FirebaseToken} = firebase_token:generate(Account, Uid, Life, Extra).
```

> **NOTE**: Extras cannot have the following keys: **acr**, **amr**, **at_hash**, **aud**, **auth_time**, **azp**, **cnf**, **c_hash**, **exp**, **firebase**, **iat**, **iss**, **jti**, **nbf**, **nonce** and **sub**.

