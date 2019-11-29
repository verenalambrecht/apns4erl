-module(apns_pools_sup).
-behaviour(supervisor).

-export([
  start_link/0,
  stop/0,
  init/1
]).

-export([
  start_child/2,
  stop_child/1,
  start_apns_pool/2,

  transaction/2
]).

-define(POOLS_TABLE, apns_pools).

start_link() ->
  ?POOLS_TABLE = ets:new(?POOLS_TABLE, [public, named_table]),
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec stop() -> ok.
stop() ->
  ets:delete(?POOLS_TABLE),
  ok.

init(_Args) ->
  SupFlags = #{
      strategy => simple_one_for_one,
      intensity => 5,
      period => 10
  },
  ChildSpecs = [
      #{
          id => start_apns_pool,
          start => {?MODULE, start_apns_pool, []},
          restart => transient,
          shutdown => 5000
      }
  ],
  {ok, {SupFlags, ChildSpecs}}.

%

start_child(ID, ConnParams) ->
  PoolName = pool_name(ID),

  ets:insert_new(?POOLS_TABLE, {PoolName, 0, ConnParams}),
  supervisor:start_child(?MODULE, [PoolName, ConnParams]).

stop_child(ID) ->
  PoolName = pool_name(ID),
  poolboy:stop(PoolName),
  ets:delete(?POOLS_TABLE, PoolName).

%

start_apns_pool(PoolName, ConnParams) ->
  PoolConfig = maps:get(pool, ConnParams, #{
    size => 5,
    overflow => 0,
    max_overflow => 0
  }),

  FuseConfig = maps:get(fuse, ConnParams, #{}),

  %

  ok = create_fuse(PoolName, FuseConfig),

  %

  PoolArgs = maps:merge(PoolConfig, #{
    name => {local, PoolName},
    worker_module => apns_connection
  }),

  WorkerArgs = maps:merge(ConnParams, #{
    pool_id => PoolName
  }),

  poolboy:start_link(maps:to_list(PoolArgs), WorkerArgs).


-spec transaction(binary(), fun((pid()) -> any())) ->
    {error, unknown_pool} | {error, not_connected} | any().
transaction(Name, Fun) ->
  PoolName = pool_name(Name),

  case fuse:ask(PoolName, sync) of
    ok ->
      poolboy:transaction(pPoolName, Fun);

    blown ->
      {error, not_connected};

    {error, not_found} ->
      {error, unknown_pool}
  end.

%

-spec pool_name(binary()) -> atom().
pool_name(Name) ->
  binary_to_atom(Name, utf8).

%

-spec create_fuse(binary(), map()) -> ok.
create_fuse(Name, Opts) ->
  Strategy = maps:get(strategy, Opts, {standard, 10, 60000}),
  Refresh = maps:get(refresh, Opts, {reset, 60000}),
  FuseOpts = {Strategy, Refresh},
  fuse:install(Name, FuseOpts),
  ok.
