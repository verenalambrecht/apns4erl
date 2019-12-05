-module(apns_pools).
-behaviour(gen_server).

-export([
  init/1,
  start_link/0,
  handle_call/3,
  stop/0
]).

-export([
  start_pool/2,
  start_apns_pool/2,
  stop_pool/1,
  stop_pool/2,
  pools/0,
  find_pool/1,
  push_notification/4,
  push_notification/5
]).

-define(POOLS_TABLE, apns_pools).

%

start_pool(Name, Params) ->
  gen_server:call(?MODULE,
                  {start_pool, Name, Params}).

stop_pool(Name) ->
  stop_pool(Name, 5000).
stop_pool(Name, Timeout) ->
  gen_server:call(?MODULE,
                  {stop_pool, Name, Timeout}).

pools() ->
    lists:foldl(fun({Name, _OnlineCount, Config}, Pools) ->
                        Pools#{Name => Config}
                end, #{}, ets:tab2list(?POOLS_TABLE)).

find_pool(Name) ->
  case ets:lookup(?POOLS_TABLE, Name) of
    [{Name, _OnlineCount, _Config} = PoolData] ->
      {ok, PoolData};

    _ ->
      {error, not_found}
  end.

push_notification(Connection, DeviceId, Notification, Headers) ->
  gen_server:call(?MODULE,
                  {push_notification,
                   Connection, DeviceId, Notification, Headers}, infinity).

push_notification(Connection, Token, DeviceId, Notification, Headers) ->
  gen_server:call(?MODULE,
                  {push_notification,
                   Connection, Token, DeviceId, Notification, Headers}, infinity).

%

init(_) ->
  {ok, []}.

start_link() ->
  ?POOLS_TABLE = ets:new(?POOLS_TABLE, [public, named_table]),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  ets:delete(?POOLS_TABLE),
  ok.

handle_call({push_notification, Connection
                              , DeviceId
                              , Notification
                              , Headers}, _, State) ->
  Result = poolboy:transaction(Connection, fun(Worker) ->
      apns_connection:push_notification(Worker
                                      , DeviceId
                                      , Notification
                                      , Headers)
    end),

  {reply, Result, State};

handle_call({push_notification, Connection
                              , Token
                              , DeviceId
                              , Notification
                              , Headers}, _, State) ->
  Result = poolboy:transaction(Connection, fun(Worker) ->
      apns_connection:push_notification(Worker
                                      , Token
                                      , DeviceId
                                      , Notification
                                      , Headers)
    end),
  {reply, Result, State};

handle_call({start_pool, Name, Params}, _, State) ->
  Result = case apns_pools:find_pool(Name) of
    {error, not_found} ->
      apns:fuse_reset(Name),
      ets:insert_new(?POOLS_TABLE, {Name, 0, Params}),
      supervisor:start_child(apns_pools_sup, [Name, Params]);

    _ ->
      {error, already_started}
  end,
  {reply, Result, State};

handle_call({stop_pool, Name, Timeout}, _, State) ->
  Result = case apns_pools:find_pool(Name) of
    {ok, {Name, _OnlineCount, _Config}} ->
      MRef = erlang:monitor(process, Name),
      poolboy:stop(Name),

      _ = receive
          {'DOWN', MRef, process, _, _} ->
            supervisor:delete_child(apns_pools_sup, Name)

          after Timeout ->
            erlang:error({not_stopping, Name})
          end,

      ets:delete(?POOLS_TABLE, Name),
      ok;

    _ -> ok
  end,
  {reply, Result, State}.

%

start_apns_pool(Name, Params) ->
  PoolConfig = maps:get(pool, Params),
  apns:fuse_reset(Name),

  %

  PoolArgs = maps:merge(PoolConfig, #{
    name => {local, Name},
    worker_module => apns_connection
  }),

  poolboy:start(maps:to_list(PoolArgs), Params).
