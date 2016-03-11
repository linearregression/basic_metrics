%%% @doc Basic VM metrics.
%%% Simple wrapper for the `exometer' metrics application to export useful
%%% metrics from an Erlang system.
%%% Handles setting up Graphite reporting and dynamic addition, subscription
%%% and updates for several exometer entry types.
-module(basic_metrics).
-export([init/0, counter/2, gauge/2, histogram/2, fast_counter/2, spiral/1, vm/1]).
-ignore_xref([init/0, counter/2, gauge/2, histogram/2, vm/0]).

-define(REPORT_INTERVAL, 5000).
-define(DEFAULT_WINDOW, 1000).
-define(APPLICATION, ?MODULE).

%% @doc Update a counter statistic.
%% If an exometer entry is not already present, create a counter and
%% subscribe to it with a pre-configured default reporter.
-spec counter(Name :: exometer:name(), Value :: number()) ->
    ok.
counter(Name, Value) ->
    Reporter = get_default_reporter(),
    ok = counter(Reporter, Name, Value).

%% @doc Update a counter statistic.
%% If an exometer entry is not already present, create a counter and
%% subscribe to it with Reporter.
-spec counter(Reporter :: atom(), Name :: exometer:name(), Value :: number()) ->
    ok. 
counter(Reporter, Name, Value) ->
    ok = exometer:update_or_create(Name, Value, counter, []),
    ok = exometer_report:subscribe(Reporter,
                                      Name, [value],
                                      ?REPORT_INTERVAL, [], true).

%% @doc Update a gauge statistic.
%% If an exometer entry is not already present, create a gauge and
%% subscribe to it with a pre-configured default reporter.
-spec gauge(Name :: exometer:name(), Value :: number()) ->
    ok.
gauge(Name, Value) -> 
    Reporter = get_default_reporter(),
    ok = gauge(Reporter, Name, Value).

%% @doc Update a gauge statistic.
%% If an exometer entry is not already present, create a gauge and
%% subscribe to it with a pre-configured default reporter.
-spec gauge(Reporter :: atom(), Name :: exometer:name(), Value :: number()) ->
    ok.
gauge(Reporter, Name, Value) -> 
    ok = exometer:update_or_create(Name, Value, gauge, []),
    ok = exometer_report:subscribe(Reporter,
                                      Name, [value],
                                      ?REPORT_INTERVAL, [], true).

%% @doc Update a histogram statistic.
%% If an exometer entry is not already present, create a histogram and
%% subscribe to it with a pre-configured default reporter.
-spec histogram(Name :: exometer:name(), Value :: number()) ->
    ok.
histogram(Name, Value) ->
    Reporter = get_default_reporter(),
    ok = histogram(Reporter, Name, Value).

%% @doc Update a histogram statistic.
%% If an exometer entry is not already present, create a histogram and
%% subscribe to it with a Reporter.
-spec histogram(Reporter :: atom(), Name :: exometer:name(), Value :: number()) ->
    ok.
histogram(Reporter, Name, Value) ->
    ok = exometer:update_or_create(Name, Value, histogram, []),
    ok = exometer_report:subscribe(Reporter,
                                      Name, [mean, 50, 75, 95, 99],
                                      ?REPORT_INTERVAL, [], true).

%% @doc Update a fast counter statistic.
%% If an exometer entry is not already present, create a fast counter and
%% subscribe to it with a pre-configured default reporter.
-spec fast_counter(Name :: exometer:name(), Value :: number()) ->
    ok.
fast_counter(Name, Value) ->
    Reporter = get_default_reporter(),
    ok = fast_counter(Reporter, Name, Value).

%% @doc Update a fast counter statistic.
%% If an exometer entry is not already present, create a fast counter and
%% subscribe to it with a Reporter.
-spec fast_counter(Reporter :: atom(), Name :: exometer:name(), Value :: number()) ->
    ok.
fast_counter(Reporter, Name, Value) ->
    ok = exometer:update_or_create(Name, Value, fast_counter, []),
    ok = exometer_report:subscribe(exometer_report_graphite_udp,
                                      Name, [value],
                                      ?REPORT_INTERVAL, [], true).

%% @doc Update a spiral statistic.
%% If an exometer entry is not already present, create a spiral and
%% subscribe to it with a pre-configured default reporter.
-spec spiral(Name :: exometer:name()) -> ok.
spiral(Name) ->
    Reporter = get_default_reporter(),
    ok = spiral(Reporter, Name).

%% @doc Update a spiral statistic.
%% If an exometer entry is not already present, create a spiral and
%% subscribe to it with Reporter.
-spec spiral(Reporter :: atom(), Name :: exometer:name()) -> ok.
spiral(Reporter, Name) ->
    Opts = [{time_span, ?DEFAULT_WINDOW}],
    ok = exometer:update_or_create(Name, 1, spiral, Opts),
    ok = exometer_report:subscribe(Reporter,
                                      Name, [value],
                                      ?REPORT_INTERVAL, [], true).

%% @doc Initialize exometer with Graphite reporting.
-spec init() ->
    ok.
init() ->
    ok = lager:notice("action=init"),
    DefaultReporter = get_default_reporter(),
    Opts = application:get_env(exometer_core, report, []),
    ReportersOpts = exometer_util:get_opt(reporters, Opts, []),
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
-spec vm(create|subscribe) ->
    ok.
vm(create) ->
    % VM memory.
    ok = exometer:new([erlang, memory],
                      {function, erlang, memory, ['$dp'], value,
                       [total, processes, system, atom, binary, ets]}),

    % Recon alloc.
    ok = exometer:new([recon, alloc],
                      {function, recon_alloc, memory, ['$dp'], value,
                       [used, allocated, unused, usage]}),

    % Recon alloc types.
    ok = exometer:new([recon, alloc, types],
                      {function, recon_alloc, memory,
                       [allocated_types], proplist,
                       [binary_alloc, driver_alloc, eheap_alloc,
                        ets_alloc, fix_alloc, ll_alloc, sl_alloc,
                        std_alloc, temp_alloc]}),

    % System process & port counts.
    ok = exometer:new([erlang, system],
                      {function, erlang, system_info, ['$dp'], value,
                       [process_count, port_count]}),

    % VM statistics.
    ok = exometer:new([erlang, statistics],
                      {function, erlang, statistics, ['$dp'], value,
                       [run_queue]}),

    ok = exometer:new([erlang, gc],
                      {function, erlang, statistics, [garbage_collection],
                       match, {total_coll, rec_wrd, '_'}}),


    ok = exometer:new([erlang, io],
                      {function, erlang, statistics, [io], match,
                       {{'_', input}, {'_', output}}});
vm(subscribe) ->
    % VM memory.
    DefaultReporter = get_default_reporter(),
    % VM memory.
    ok = exometer_report:subscribe(DefaultReporter ,
                                   [recon, alloc],
                                   [used, allocated, unused, usage], ?REPORT_INTERVAL,
                                   [], true),

    % Recon alloc.
    ok = exometer_report:subscribe(DefaultReporter ,
                                   [recon, alloc, types],
                                   [binary_alloc, driver_alloc, eheap_alloc,
                                    ets_alloc, fix_alloc, ll_alloc, sl_alloc,
                                    std_alloc, temp_alloc], ?REPORT_INTERVAL,
                                   [], true),

    % System process & port counts.
    ok = exometer_report:subscribe(DefaultReporter ,
                                   [erlang, system],
                                   [process_count, port_count], ?REPORT_INTERVAL,
                                   [], true),
    % VM statistics.
    ok = exometer_report:subscribe(DefaultReporter ,
                                   [erlang, statistics],
                                   [run_queue], ?REPORT_INTERVAL, [], true),
    ok = exometer_report:subscribe(DefaultReporter ,
                                   [erlang, gc],
                                   [total_coll, rec_wrd], ?REPORT_INTERVAL, [], true),
    ok = exometer_report:subscribe(DefaultReporter ,
                                   [erlang, io],
                                   [input, output], ?REPORT_INTERVAL, [], true);
vm(Arg) -> {error, {Arg, badarg}}.

get_default_reporter() ->
    application:get_env(?APPLICATION, default_metrics_reporter, undefined).

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
