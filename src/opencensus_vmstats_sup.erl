-module(opencensus_vmstats_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
  supervisor:start_link(?MODULE, []).

init(_) ->
  AppOptions = application:get_all_env(opencensus_vmstats),
  VMstatsOptions = [{key_separator, $/} | AppOptions],
  ChildSpecs = [
                {vmstats,
                 {vmstats_server,
                  start_link,
                  [opencensus_vmstats_sink, "vmstat", VMstatsOptions]},
                 permanent,
                 1000,
                 worker,
                 [vmstats_server]}
               ],
  {ok, {{one_for_one, 5, 3600}, ChildSpecs}}.
