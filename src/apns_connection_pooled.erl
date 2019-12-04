-module(apns_connection_pooled).

% simple wrapper for apns_connection to allow poolboy to pass 2nd parameter

-export([start_link/1]).

start_link({Connection, Client}) ->
  apns_connection:start_link(Connection, Client).