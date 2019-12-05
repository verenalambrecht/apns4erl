%%% @doc Main module for apns4erl API. Use this one from your own applications.
%%%
%%% Copyright 2017 Erlang Solutions Ltd.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% @end
%%% @copyright Inaka <hello@inaka.net>
%%%
-module(apns).
-author("Felipe Ripoll <felipe@inakanetworks.com>").

%% API
-export([ start/0
        , stop/0
        , connect/1
        , connect/2
        , wait_for_connection_up/1
        , close_connection/1
        , push_notification/3
        , push_notification/4
        , push_notification_token/4
        , push_notification_token/5
        , default_headers/0
        , generate_token/2
        , generate_token/3
        , get_feedback/0
        , get_feedback/1
        , fuse_melt/1
        , fuse_reset/1
        ]).

-export_type([ json/0
             , device_id/0
             , response/0
             , token/0
             , headers/0
             ]).

-type json()      :: #{binary() => binary() | json()}.
-type device_id() :: binary().
-type response()  :: { integer()          % HTTP2 Code
                     , [term()]           % Response Headers
                     , [term()] | no_body % Response Body
                     } | timeout.
-type token()     :: binary().
-type headers()   :: #{ apns_id          => binary()
                      , apns_expiration  => binary()
                      , apns_priority    => binary()
                      , apns_topic       => binary()
                      , apns_collapse_id => binary()
                      , apns_push_type   => binary()
                      , apns_auth_token  => binary()
                      }.
-type feedback()  :: apns_feedback:feedback().

-define(POOLS_TABLE, apns_pools).

-include("apns.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Used when starting the application on the shell.
-spec start() -> ok.
start() ->
  {ok, _} = application:ensure_all_started(apns),
  ok.

%% @doc Stops the Application
-spec stop() -> ok.
stop() ->
  ok = application:stop(apns),
  ok.

%% @doc Connects to APNs service with Provider Certificate or Token
-spec connect( apns_connection:type(), apns_connection:name()) ->
  {ok, pid()}.
connect(Type, ConnectionName) ->
  DefaultConnection = apns_connection:default_connection(Type, ConnectionName),
  Conn = case ConnectionName of
    undefined -> maps:without([name], DefaultConnection);
    _ -> DefaultConnection
  end,
  connect(Conn).

%% @doc Connects to APNs service
-spec connect(apns_connection:connection()) -> {ok, pid()}.
connect(Connection) ->
  Name = maps:get(name, Connection, undefined),
  Pool = maps:get(pool, Connection, undefined),
  Fuse = maps:get(fuse, Connection, {
                                      {standard, 5, 60000},
                                      {reset, 60000}
                                    }),


  case Name of
    undefined ->
      % nameless connection
      apns_sup:create_connection(Connection#{ fuse => undefined });

    Name ->
      fuse_create(Name, Fuse),

      case Pool of
        undefined ->
          apns_sup:create_connection(Connection#{ fuse => Name });

        Pool ->
          %  pooled connections should be nameless
          apns_pools:start_pool(Name, maps:without([name], Connection#{ fuse => Name }))
      end
  end.

%% @doc Wait for the APNs connection to be up.
-spec wait_for_connection_up(pid()) -> ok.
wait_for_connection_up(Server) ->
  apns_connection:wait_apns_connection_up(Server).

%% @doc Closes the connection with APNs service.
-spec close_connection(apns_connection:name() | pid()) -> ok.
close_connection(ConnectionId) when is_pid(ConnectionId) ->
  apns_connection:close_connection(ConnectionId);

close_connection(ConnectionId) ->
  fuse_remove(ConnectionId),
  case apns_pools:find_pool(ConnectionId) of
    {ok, _} ->
      apns_pools:stop_pool(ConnectionId);

    {error, not_found} ->
      apns_connection:close_connection(ConnectionId)
  end.

-spec fuse_create(atom(), any()) -> ok.
fuse_create(Name, Config) ->
  fuse:install(Name, Config).

-spec fuse_remove(atom()) -> ok.
fuse_remove(Name) ->
  fuse:remove(Name).

-spec fuse_melt(atom()) -> ok.
fuse_melt(Name) ->
  fuse:melt(Name).

-spec fuse_reset(atom()) -> ok.
fuse_reset(Name) ->
  fuse:reset(Name).

%% @doc Push notification to APNs. It will use the headers provided on the
%%      environment variables.
-spec push_notification( apns_connection:name() | pid() | binary()
                       , device_id()
                       , json()
                       ) -> response() | {error, not_connection_owner}.
push_notification(ConnectionId, DeviceId, JSONMap) ->
  Headers = default_headers(),
  push_notification(ConnectionId, DeviceId, JSONMap, Headers).

%% @doc Push notification to certificate APNs Connection.
-spec push_notification( apns_connection:name() | pid() | binary()
                       , device_id()
                       , json()
                       , headers()
                       ) -> response() | {error, not_connection_owner}.
push_notification(ConnectionId, DeviceId, JSONMap, Headers) ->
  Notification = jsx:encode(JSONMap),
  do_push(ConnectionId, #{ device_id => DeviceId, notification => Notification, headers => Headers }).

%% @doc Push notification to APNs with authentication token. It will use the
%%      headers provided on the environment variables.
-spec push_notification_token( apns_connection:name() | pid()
                             , token()
                             , device_id()
                             , json()
                             ) -> response() | {error, not_connection_owner}.
push_notification_token(ConnectionId, Token, DeviceId, JSONMap) ->
  Headers = default_headers(),
  push_notification_token(ConnectionId, Token, DeviceId, JSONMap, Headers).

%% @doc Push notification to authentication token APNs Connection.
-spec push_notification_token( apns_connection:name() | pid()
                             , token()
                             , device_id()
                             , json()
                             , headers()
                             ) -> response() | {error, not_connection_owner}.
push_notification_token(ConnectionId, Token, DeviceId, JSONMap, Headers) ->
  Notification = jsx:encode(JSONMap),
  do_push(ConnectionId,  #{ device_id => DeviceId,
                            notification => Notification,
                            headers => Headers,
                            token => Token }).

-spec do_push(apns_connection:name(), list()) -> ok | {error, not_connected} | {error, unknown_pool}.
do_push(Name, Params) when is_pid(Name) ->
  apply(apns_connection, push_notification, Params#{ start => ?MNOW });
do_push(Name, Params) when is_atom(Name) ->
  case apns_pools:find_pool(Name) of
    {ok, _} ->
      case fuse:ask(Name, sync) of
        ok ->
          apply(apns_pools, push_notification, Params#{ start => ?MNOW });

        blown ->
          {error, not_connected};

        {error, not_found} ->
          {error, unknown_pool}
      end;

    {error, not_found} ->
      case fuse:ask(Name, sync) of
        ok ->
          apply(apns_connection, push_notification, Params#{ start => ?MNOW });
        blown ->
          {error, not_connected};

        {error, not_found} ->
          {error, unknown_connection}
      end
  end.






-spec generate_token(binary(), binary()) -> token().
generate_token(TeamId, KeyId) ->
  {ok, KeyPath} = application:get_env(apns, token_keyfile),
  generate_token(TeamId, KeyId, KeyPath).

-spec generate_token(binary(), binary(), string()) -> token().
generate_token(TeamId, KeyId, KeyPath) ->
  Algorithm = <<"ES256">>,
  Header = jsx:encode([ {alg, Algorithm}
                      , {typ, <<"JWT">>}
                      , {kid, KeyId}
                      ]),
  Payload = jsx:encode([ {iss, TeamId}
                       , {iat, apns_utils:epoch()}
                       ]),
  HeaderEncoded = base64url:encode(Header),
  PayloadEncoded = base64url:encode(Payload),
  DataEncoded = <<HeaderEncoded/binary, $., PayloadEncoded/binary>>,
  Signature = apns_utils:sign(DataEncoded, KeyPath),
  <<DataEncoded/binary, $., Signature/binary>>.

%% @doc Get the default headers from environment variables.
-spec default_headers() -> apns:headers().
default_headers() ->
  Headers = [ apns_id
            , apns_expiration
            , apns_priority
            , apns_topic
            , apns_collapse_id
            , apns_push_type
            ],

  %% The apns_push_type key is required starting from iOS 13.
  default_headers(Headers, #{apns_push_type => <<"alert">>}).

%% Requests for feedback to APNs. This requires Provider Certificate.
-spec get_feedback() -> [feedback()] | {error, term()} | timeout.
get_feedback() ->
  {ok, Host} = application:get_env(apns, feedback_host),
  {ok, Port} = application:get_env(apns, feedback_port),
  {ok, Certfile} = application:get_env(apns, certfile),
  Keyfile = application:get_env(apns, keyfile, undefined),
  {ok, Timeout} = application:get_env(apns, timeout),
  Config = #{ host     => Host
            , port     => Port
            , certfile => Certfile
            , keyfile  => Keyfile
            , timeout  => Timeout
            },
  get_feedback(Config).

%% Requests for feedback to APNs. This requires Provider Certificate.
-spec get_feedback(apns_feedback:feedback_config()) -> [feedback()] | {error, term()} | timeout.
get_feedback(Config) ->
  apns_feedback:get_feedback(Config).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% Build a headers() structure from environment variables.
-spec default_headers(list(), headers()) -> headers().
default_headers([], Headers) ->
  Headers;
default_headers([Key | Keys], Headers) ->
  case application:get_env(apns, Key, undefined) of
    undefined ->
      default_headers(Keys, Headers);
    Value ->
      NewHeaders = Headers#{Key => to_binary(Value)},
      default_headers(Keys, NewHeaders)
  end.

%% Convert to binary
to_binary(Value) when is_integer(Value) ->
  list_to_binary(integer_to_list(Value));
to_binary(Value) when is_list(Value) ->
  list_to_binary(Value);
to_binary(Value) when is_binary(Value) ->
  Value.
