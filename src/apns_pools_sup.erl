-module(apns_pools_sup).
-behaviour(supervisor).

-export([
  start_link/0,
  stop/0,
  init/1
]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec stop() -> ok.
stop() ->
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
          start => {apns_pools, start_apns_pool, []},
          restart => transient,
          shutdown => 5000
      }
  ],
  {ok, {SupFlags, ChildSpecs}}.
