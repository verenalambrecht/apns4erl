-module(apns_pool).

-export([
  create_pool/2,
  destroy_pool/1,
  transaction/2
]).

-type pool_name() :: {via, gproc, {n, l, {apns_connection, binary()}}}.

-spec create_pool(any(), map()) -> pid().
create_pool(Name, Connection0) ->
  #{
    pool_config := #{
      size := Size
    }
  } = Connection0,

  Connection = Connection0#{ fuse => fuse_name(Name)},

  create_fuse(Name, Connection),

  PoolArgs = [
      {name, pool_name(Name)},
      {worker_module, apns_connection},
      {size, Size}
  ],
  poolboy:start_link(PoolArgs, Connection).

destroy_pool(Name) ->
  case find_pool(Name) of
    undefined -> ok;
    Pid -> poolboy:stop(Pid)
  end.

-spec transaction(binary(), fun((pid()) -> any())) ->
    {error, unknown_pool} | {error, not_connected} |any().
transaction(Name, Fun) ->
  case fuse:ask(fuse_name(Name), sync) of
    ok ->
      poolboy:transaction(find_pool(Name), Fun);

    blown ->
      {error, not_connected};

    {error, not_found} ->
      {error, unknown_pool}
  end.

-spec find_pool(binary()) -> pid().
find_pool(Name) ->
  gproc:where({n, l, {apns_connection, Name}}).

-spec pool_name(binary()) -> pool_name().
pool_name(Name) ->
  {via, gproc, {n, l, {apns_connection, Name}}}.

-spec create_fuse(binary(), map()) -> ok.
create_fuse(Name, Opts) ->
  Strategy = maps:get(fuse_strategy, Opts, {standard, 10, 60000}),
  Refresh = maps:get(fuse_refresh, Opts, {reset, 60000}),
  FuseOpts = {Strategy, Refresh},
  fuse:install(fuse_name(Name), FuseOpts),
  ok.

-spec fuse_name(binary()) -> atom().
fuse_name(PoolName) ->
  % TODO: avoid to generate atoms
  binary_to_atom(PoolName, utf8).
