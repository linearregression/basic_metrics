%%% @doc Basic VM metrics.
%%% Simple wrapper for the `exometer' metrics application to export useful
%%% metrics from an Erlang system.
%%% Handles setting up Graphite reporting and dynamic addition, subscription
%%% and updates for several exometer entry types.
-module(basic_metrics).
-export([init/0, counter/2, gauge/2, histogram/2, fast_counter/2, spiral/1, vm/0]).
-ignore_xref([init/0, counter/2, gauge/2, histogram/2, vm/0]).

-define(REPORT_INTERVAL, 5000).
-define(DEFAULT_WINDOW, 1000).
-define(APPLICATION, ?MODULE).

%% @doc Update a counter statistic.
%% If an exometer entry is not already present, create a counter and
%% subscribe to it with `(exometer_report_graphite_udp'.
-spec counter(Name :: exometer:name(), Value :: number()) ->
    ok.
counter(Name, Value) ->
    ok = exometer:update_or_create(Name, Value, counter, []),
    ok = exometer_report:subscribe(exometer_report_graphite_udp,
                                      Name, [value],
                                      ?REPORT_INTERVAL, [], true).

%% @doc Update a gauge statistic.
%% If an exometer entry is not already present, create a gauge and
%% subscribe to it with `(exometer_report_graphite_udp'.
-spec gauge(Name :: exometer:name(), Value :: number()) ->
    ok.
gauge(Name, Value) -> 
    ok = exometer:update_or_create(Name, Value, gauge, []),
    ok = exometer_report:subscribe(exometer_report_graphite_udp,
                                      Name, [value],
                                      ?REPORT_INTERVAL, [], true).

%% @doc Update a histogram statistic.
%% If an exometer entry is not already present, create a histogram and
%% subscribe to it with (exometer_report_graphite_udp.
-spec histogram(Name :: exometer:name(), Value :: number()) ->
    ok.
histogram(Name, Value) ->
    ok = exometer:update_or_create(Name, Value, histogram, []),
    ok = exometer_report:subscribe(exometer_report_graphite_udp,
                                      Name, [mean, 50, 75, 95, 99],
                                      ?REPORT_INTERVAL, [], true).


%% @doc Update a fast counter statistic.
%% If an exometer entry is not already present, create a fast counter and
%% subscribe to it with (exometer_report_graphite_udp.
fast_counter(Name, Value) ->
    ok = exometer:update_or_create(Name, Value, fast_counter, []),
    ok = exometer_report:subscribe(exometer_report_graphite_udp,
                                      Name, [value],
                                      ?REPORT_INTERVAL, [], true).

%% @doc Update a spiral statistic.
%% If an exometer entry is not already present, create a spiral and
%% subscribe to it with (exometer_report_graphite_udp.
spiral(Name) ->
    Opts = [{time_span, ?DEFAULT_WINDOW}],
    ok = exometer:update_or_create(Name, 1, spiral, Opts),
    ok = exometer_report:subscribe(exometer_report_graphite_udp,
                                      Name, [value],
                                      ?REPORT_INTERVAL, [], true).

%% @doc Initialize exometer with Graphite reporting.
-spec init() ->
    ok.
init() ->
    ok = lager:notice("action=init"),
    DefaultReporter = application:get_env(?APPLICATION, default_metrics_reporter, undefined),
    Opts = application:get_env(exometer_core, report, []),
    ReportersOpts = get_opt(reporters, Opts, []),
    DefaultReporterOpt = get_opt(DefaultReporter, ReportersOpts), 
    DefaultReporterOpt1 = lists:flatten(DefaultReporterOpt),
    ok = add_reporter(exometer_report:add_reporter(DefaultReporter, DefaultReporterOpt1)),
    ok.

%% @doc Initialize basic VM metrics.
%% The following metrics are exported:
%% <ul>
%% <li>erlang:memory/1:
%%     `erlang.memory.{total, processes, system, atom, binary, ets}'
%%     <p>VM memory.
%%     <ul>
%%      <li>total = processes + system.</li>
%%      <li>processes = used by Erlang processes, their stacks and heaps.</li>
%%      <li>system = used but not directly related to any Erlang process.</li>
%%      <li>atom = allocated for atoms (included in system).</li>
%%      <li>binary = allocated for binaries (included in system).</li>
%%      <li>ets = allocated for ETS tables (included in system).</li>
%%     </ul>
%%     </p>
%% </li>
%% <li>recon_alloc:memory/1:
%%     `recon.alloc.memory.{used, allocated, unused, usage}'
%%     <p>Memory actively used by the VM, allocated (should ~match OS
%%     allocation), unused (i.e. allocated - used), and usage
%%     (used / allocated).</p>
%% </li>
%% <li>recon_alloc:memory(allocated_types):
%%     `recon.alloc.memory.types.{binary_alloc, ...}'
%%     <p>Memory reserved by the VM, grouped into different utility
%%     allocators.</p>
%% </li>
%% <li>erlang:system_info/1:
%%     `erlang.system.{process_count, port_count}'
%%     <p>System process and port counts.
%%     <ul>
%%       <li>process_count = current number of processes.</li>
%%       <li>port_count = current number of ports.</li>
%%     </ul>
%%     </p>
%% </li>
%% <li>erlang:statistics/1:
%%     `erlang.statistics.run_queue',
%%     `erlang.gc.{total_coll, rec_wrd}',
%%     `erlang.io.{input, output}'
%%     <p>VM statistics.
%%     <ul>
%%      <li>The number of processes that are ready to run on all available run
%%          queues.</li>
%%      <li>Total garbage collections and words reclaimed.</li>
%%      <li>Total bytes input and output through ports.</li>
%%     </ul>
%%     </p>
%% </li>
%% </ul>
-spec vm() ->
    ok.
vm() ->
    % VM memory.
    ok = exometer:new([erlang, memory],
                      {function, erlang, memory, ['$dp'], value,
                       [total, processes, system, atom, binary, ets]}),
    ok = exometer_report:subscribe(exometer_report_graphite_udp,
                                   [erlang, memory],
                                   [total, processes, system, atom, binary,
                                    ets], ?REPORT_INTERVAL, [], true),

    % Recon alloc.
    ok = exometer:new([recon, alloc],
                      {function, recon_alloc, memory, ['$dp'], value,
                       [used, allocated, unused, usage]}),
    ok = exometer_report:subscribe(exometer_report_graphite_udp,
                                   [recon, alloc],
                                   [used, allocated, unused, usage], ?REPORT_INTERVAL,
                                   [], true),

    % Recon alloc types.
    ok = exometer:new([recon, alloc, types],
                      {function, recon_alloc, memory,
                       [allocated_types], proplist,
                       [binary_alloc, driver_alloc, eheap_alloc,
                        ets_alloc, fix_alloc, ll_alloc, sl_alloc,
                        std_alloc, temp_alloc]}),
    ok = exometer_report:subscribe(exometer_report_graphite_udp,
                                   [recon, alloc, types],
                                   [binary_alloc, driver_alloc, eheap_alloc,
                                    ets_alloc, fix_alloc, ll_alloc, sl_alloc,
                                    std_alloc, temp_alloc], ?REPORT_INTERVAL,
                                   [], true),

    % System process & port counts.
    ok = exometer:new([erlang, system],
                      {function, erlang, system_info, ['$dp'], value,
                       [process_count, port_count]}),
    ok = exometer_report:subscribe(exometer_report_graphite_udp,
                                   [erlang, system],
                                   [process_count, port_count], ?REPORT_INTERVAL,
                                   [], true),

    % VM statistics.
    ok = exometer:new([erlang, statistics],
                      {function, erlang, statistics, ['$dp'], value,
                       [run_queue]}),
    ok = exometer_report:subscribe(exometer_report_graphite_udp,
                                   [erlang, statistics],
                                   [run_queue], ?REPORT_INTERVAL, [], true),

    ok = exometer:new([erlang, gc],
                      {function, erlang, statistics, [garbage_collection],
                       match, {total_coll, rec_wrd, '_'}}),
    ok = exometer_report:subscribe(exometer_report_graphite_udp,
                                   [erlang, gc],
                                   [total_coll, rec_wrd], ?REPORT_INTERVAL, [], true),

    ok = exometer:new([erlang, io],
                      {function, erlang, statistics, [io], match,
                       {{'_', input}, {'_', output}}}),
    ok = exometer_report:subscribe(exometer_report_graphite_udp,
                                   [erlang, io],
                                   [input, output], ?REPORT_INTERVAL, [], true).

get_opt(K, Opts) ->
    case lists:keyfind(K, 1, Opts) of
        {_, V} -> V;
        false  -> error({required, K})
    end.

get_opt(K, Opts, Default) ->
    exometer_util:get_opt(K, Opts, Default).

add_reporter(ok) ->
    ok;
add_reporter({error, already_running}) ->
    ok;
add_reporter({error, E}) ->
    {error, E}.
