-module(pooled_connection_SUITE).

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        ]).

-export([
  create_pool_of_connections/1
]).

-type config() :: [{atom(), term()}].

%

-spec all() -> [atom()].
all() ->  [
  create_pool_of_connections
].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  ok = apns:start(),
  {ok, _} = application:ensure_all_started(poolboy),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  ok = apns:stop(),
  Config.

%

-spec create_pool_of_connections(config()) -> ok.
create_pool_of_connections(_Config) ->
  ok = mock_gun_open(),
  PoolSize = 3,
  Connection = #{
      pool => #{
          size => PoolSize
      },
      apple_host => <<"apple-host">>,
      apple_port => <<"apple-port">>,
      keydata => <<"keydata">>,
      certdata => <<"certdata">>,
      timeout => 5000,
      type => certdata,
      proxy_info =>
        #{type => connect, host => <<"proxy-host">>, port => <<"proxy-port">>}
  },
  PoolName = <<"pooled_apns_connection">>,
  {ok, PoolPid} = apns:connect_pooled(PoolName, Connection),
  {ready, PoolSize, _, _} = poolboy:status(PoolPid),
  apns:disconnect_pooled(PoolName),
  false = is_process_alive(PoolPid),
  [_] = meck:unload().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec test_function() -> ok.
test_function() ->
  receive
    normal           -> ok;
    {crash, Pid}     -> Pid ! {gun_down, self(), http2, closed, [], []};
    _                -> test_function()
  end.

-spec mock_gun_open() -> ok.
mock_gun_open() ->
  meck:expect(gun, open, fun(_, _, _) ->
    GunPid = spawn(fun test_function/0),
    self() ! {gun_up, GunPid, http2},
    {ok, GunPid}
  end).

% -spec mock_gun_post() -> ok.
% mock_gun_post() ->
%   meck:expect(gun, post, fun(_, _, _, _) ->
%     make_ref()
%   end).

% -spec mock_gun_await(term()) -> ok.
% mock_gun_await(Result) ->
%   meck:expect(gun, await, fun(_, _, _) ->
%     Result
%   end).

% -spec mock_gun_await_body(term()) -> ok.
% mock_gun_await_body(Body) ->
%   meck:expect(gun, await_body, fun(_, _, _) ->
%     {ok, Body}
%   end).

% -spec maybe_mock_apns_os() -> ok.
% maybe_mock_apns_os() ->
%   %% @TODO: Add logic to validate if the user wants to avoid to mock this call,
%   %% and make the real call with real files instead.
%   meck:expect(apns_os, cmd, fun(_) ->
%     {0, "12345678"}
%   end).
