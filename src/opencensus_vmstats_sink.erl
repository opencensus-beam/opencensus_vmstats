-module(opencensus_vmstats_sink).

-behaviour(vmstats_sink).

%% API exports
-export([collect/3]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% API functions
%%====================================================================

collect(_Type, Key, Value) ->
  try
    Measure = erlang:binary_to_atom(erlang:list_to_binary(Key), utf8),
    oc_stat:record(tags(), Measure, Value),
    ok
  catch
    error:{unknown_measure, Name} ->
      ?LOG_ERROR("opencensus_vmstats: unknown metric ~p,"
                 " have you ran `opencensus_vmstats:setup_metrics()`?", [Name]);
    badarg ->
      ?LOG_ERROR("opencensus_vmstats: unknown metric ~p,"
                 " have you ran `opencensus_vmstats:setup_metrics()`?", [Key])
  end.

%%====================================================================
%% Internal functions
%%====================================================================

tags() ->
  PreDef = #{host => erlang:system_info(machine),
             node => erlang:atom_to_binary(node(), utf8),
             otp_release => erlang:system_info(otp_release),
             system_version => erlang:system_info(system_version),
             system_architecture => erlang:system_info(system_architecture)},
  Env = application:get_env(opencensus_vmstats, tags, #{}),

  maps:merge(PreDef, Env).
