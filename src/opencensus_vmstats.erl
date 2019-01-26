-module(opencensus_vmstats).
-behaviour(application).

-export([setup_metrics/0, setup_views/0]).

-export([start/2, stop/1]).

-include_lib("kernel/include/logger.hrl").

setup_metrics() ->
  [
   % Processes
   oc_stat_measure:new(
     'vmstat/proc_count',
     "Number of processes currently existing at the local node",
     proc),
   oc_stat_measure:new(
     'vmstat/proc_limit',
     "Maximum number of simultaneously existing processes at the local node",
     proc),

   % Ports
   oc_stat_measure:new(
     'vmstat/port_count',
     "Number of ports currently existing at the local node",
     port),
   oc_stat_measure:new(
     'vmstat/port_limit',
     "Maximum number of simultaneously existing ports at the local node",
     port),

   % Atoms
   oc_stat_measure:new(
     'vmstat/atom_count',
     "Number of atoms currently existing at the local node",
     atom),

   % Messages
   oc_stat_measure:new(
     'vmstat/messages_in_queues',
     "Total number of messages in all queues at the local node",
     message),

   % Modules
   oc_stat_measure:new(
     'vmstat/modules',
     "Total number of modules registered at the local node",
     module),

   % Process queue
   oc_stat_measure:new(
     'vmstat/run_queue',
     "Total length of all normal run-queues",
     process),

   % Uptime
   oc_stat_measure:new(
     'vmstat/vm_uptime',
     "Uptime of the local node",
     second)
  ].

setup_views() -> ok.

start(_Type, _Args) ->
  opencensus_vmstats_sup:start_link().

stop(_) -> ok.
