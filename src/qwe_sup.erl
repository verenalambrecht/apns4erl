-module(qwe_sup).
-behaviour(supervisor).

-export([
  start_link/0,
  init/1
]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
  SupFlags = #{
      strategy => one_for_one,
      intensity => 5,
      period => 10,
      type => transient
  },

  Children = [#{
        id => apns_sup,
        start => {apns_sup, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => supervisor
    }, #{
        id => apns_pools_sup,
        start => {apns_pools_sup, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => supervisor
    }, #{
        id => apns_pools,
        start => {apns_pools, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker
    }],

  {ok, {SupFlags, Children}}.
