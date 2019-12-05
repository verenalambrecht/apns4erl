-module(apns_instrumenter).

-export([setup/0]).

-export([deregister_cleanup/1,
         collect_mf/2]).

-ifdef(TEST).
-export([metrics/0]).
-endif.

-import(prometheus_model_helpers, [create_mf/4]).

-include_lib("prometheus/include/prometheus.hrl").

-define(METRIC_NAME_PREFIX, "apns_").

setup() ->
    prometheus_counter:declare([{name, apns_pool_reconnects_total},
                                {labels, [name, result]},
                                {help, "Pool reconnects counter."}]),
    prometheus_counter:declare([{name, apns_pool_requests_total},
                                {labels, [name]},
                                {help, "Pool requests counter."}]),
    prometheus_counter:declare([{name, apns_workers_started_total},
                                {labels, [name]},
                                {help, "Total number of worker processes started by this pool."}]),

    %% if too many timeout checkout/call timeouts, perhaps pool settings could be reviewed.
    prometheus_counter:declare([{name, apns_requests_total},
                                {labels, [name, result]},
                                {help, "Counting requests on apns side"}]),
    %% if pool requests waits too long, perhaps connection isn't good or
    %% pool settings could be reviewed.
    %% currently start before checkout, if gen_server:call times out finishes in
    %% catch. If gen_server:call is ok, finishes inside apns_conn call handler..
    prometheus_summary:declare([{name, apns_requests_waiting_microseconds},
                                {labels, [name, result]},
                                {help, "Counting requests waiting on apns side"}]),

    prometheus_registry:register_collector(?MODULE).

%% @private
deregister_cleanup(_) -> ok.

-spec collect_mf(_Registry, Callback) -> ok when
      _Registry :: prometheus_registry:registry(),
      Callback :: prometheus_collector:callback().
%% @private
collect_mf(_Registry, Callback) ->
    Metrics = metrics(),
    _ = [add_metric_family(Metric, Callback) || Metric <- Metrics],
    ok.

add_metric_family({Name, Type, Help, Metrics}, Callback) ->
    Callback(create_mf(?METRIC_NAME(Name), Help, Type, Metrics)).

metrics() ->
    Pools = apns_pools:pools(),
    MetricsBlank = #{online => [],
                     size => [],
                     max_overflow => [],
                     state => [],
                     free_fixed_workers => [],
                     overflow_workers => [],
                     workers => []},
    Stat = maps:fold(
        fun(Name, #{pool_args := PoolArgs},
            #{online := OldOnline,
            size := OldSize,
            max_overflow := OldMaxOverflow,
            state := OldState,
            free_fixed_workers := OldFFW,
            overflow_workers := OldOverflowWorkers,
            workers := OldWorkers}) ->

            %% TODO: consider using apns:status here
            Size = proplists:get_value(size, PoolArgs),
            MaxOverflow = proplists:get_value(max_overflow, PoolArgs),

            try poolboy:status(Name) of
                {State, FreeFixedWorkers, OverflowWorkers, Workers} ->
                    #{online => [{[{name, Name}], fuse:ask(Name, async_dirty) =:= ok} | OldOnline],
                    size => [{[{name, Name}], Size} | OldSize],
                    max_overflow => [{[{name, Name}], MaxOverflow} | OldMaxOverflow],
                    state => [{[{name, Name}], pool_state(Name, State)} | OldState],
                    free_fixed_workers => [{[{name, Name}], FreeFixedWorkers} | OldFFW],
                    overflow_workers => [{[{name, Name}], OverflowWorkers} | OldOverflowWorkers],
                    workers => [{[{name, Name}], Workers} | OldWorkers]}
            catch
                exit:{timeout, {gen_server, call, [Name, _]}} ->
                    #{online => [{[{name, Name}], fuse:ask(Name, async_dirty) =:= ok} | OldOnline],
                    size => [{[{name, Name}], Size} | OldSize],
                    max_overflow => [{[{name, Name}], MaxOverflow} | OldMaxOverflow],
                    state => [{[{name, Name}], undefined} | OldState],
                    free_fixed_workers => [{[{name, Name}], undefined} | OldFFW],
                    overflow_workers => [{[{name, Name}], undefined} | OldOverflowWorkers],
                    workers => [{[{name, Name}], undefined} | OldWorkers]}
            end
        end,
        MetricsBlank, Pools),

    [{pools, gauge,
      "Number of configured pools.",
      maps:size(Pools)},
     {pool_online, boolean,
      "Is pool online",
      maps:get(online, Stat)},
     {pool_fixed_size, gauge,
      "Fixed pool size.",
      maps:get(size, Stat)},
     {pool_max_overflow, gauge,
      "Maximum number of workers created if fixed size pool is empty.",
      maps:get(max_overflow, Stat)},
     {pool_workers_checked_out, gauge,
      "Current number of checked-out workers",
      maps:get(workers, Stat)},
     {pool_state, untyped,
      "Pool state: 0 - offline, 1 - ready, 2 - overflow, 3 - full.",
      maps:get(state, Stat)}].

pool_state(Name, State) ->
    case fuse:ask(Name, async_dirty) of
        ok -> pool_state(State);
        _ -> 0
    end.

pool_state(ready) ->
    1;
pool_state(overflow) ->
    2;
pool_state(full) ->
    3.
