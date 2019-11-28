-module(apns_pool).

-export([
  create_pool/2,
  destroy_pool/1,
  transaction/2
]).

-type pool_name() :: {via, gproc, {n, l, {apns_connection, binary()}}}.

-spec create_pool(any(), map()) -> pid().
create_pool(Name, Connection) ->
  #{
    pool_config := #{
      size := Size
    }
  } = Connection,

  PoolArgs = [
      {name, pool_name(Name)},
      {worker_module, apns_connection},
      {size, Size}
  ],
  WorkerArgs = Connection,
  poolboy:start_link(PoolArgs, WorkerArgs).

destroy_pool(Name) ->
  case find_pool(Name) of
    undefined -> ok;
    Pid -> poolboy:stop(Pid)
  end.

-spec transaction(any(), fun((pid()) -> any())) -> {error, pool_not_found} | any().
transaction(Name, Fun) ->
  try
      poolboy:transaction(find_pool(Name), Fun)
  catch
      exit:{noproc, _} ->
          {error, pool_not_found}
  end.

-spec find_pool(any()) -> pid().
find_pool(Name) ->
  gproc:where({n, l, {apns_connection, Name}}).

-spec pool_name(binary()) -> pool_name().
pool_name(Name) ->
  {via, gproc, {n, l, {apns_connection, Name}}}.
