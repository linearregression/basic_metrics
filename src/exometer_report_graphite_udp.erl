%%% @doc Basic exometer reporter backend to graphite.
%%%
%%% A simple `gen_server' consumes metrics from exometer and posts
%%% to graphite using Carbon plaintext protocol via udp.
%%% <a href="http://graphite.readthedocs.org/en/latest/feeding-carbon.html">
%%% Graphite Carbon plaintext protocol</a>
%%% 
-module(exometer_report_graphite_udp).
%% exometer_report callbacks
-export(
   [
    exometer_init/1,
    exometer_info/2,
    exometer_cast/2,
    exometer_call/3,
    exometer_report/5,
    exometer_subscribe/5,
    exometer_unsubscribe/4,
    exometer_newentry/2,
    exometer_setopts/4,
    exometer_terminate/2
   ]).

-include_lib("exometer_core/include/exometer.hrl").

-define(DEFAULT_HOST, "9.9.9.9").
-define(DEFAULT_PORT, 9999).
-define(DEFAULT_CONNECT_TIMEOUT, 5000).
-define(DEFAULT_UDP_OPTS,[]).

-record(state, {
          socket = undefined,
          host_ip = undefined,
          port = ?DEFAULT_PORT,
          protocol = udp,
          path = undefined,
          retry_failed_metrics = false,
          interval = 1000,
          send_timeout = 5000,
          reconnect = 5000
       }).

-type mod_state() :: #state{}.

%% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).
-define(UNIX_EPOCH, 62167219200).

%%% ===================================================
%%% Public API
%%% ===================================================
%% @doc exometer_init
%% exometer_report callback
%% Set a proper initial state. All configurations read in to memory.
%% @end
-spec exometer_init(exometer_report:options()) -> exometer_report:callback_result().
exometer_init(Opts) ->
    ok = lager:notice("action=init"),
    ok = lager:info("action=~s_init ~s", [?MODULE, Opts]),
    HostIp = get_opt(host_ip, Opts),
    {ok, HostIp1} = inet:parse_address(HostIp),
    Port = get_opt(port, Opts, ?DEFAULT_PORT),
    %Port1 = erlang:list_to_integer(Port),
    %Protocol = erlang:to_atom(protocol, Opts, udp),
    Protocol = udp,
    MetricPath = get_opt(metric_path, Opts, undefined),
    RetryFailedMetrics = get_opt(retry_failed_metrics, Opts),
    %Interval = erlang:to_integer(interval, Opts, 1000),
    SendTimeout = get_opt(retry_failed_metrics, Opts),
    ReconnectInterval =get_opt(reconnect, Opts),
    {ok, Socket} = connect(),
    {ok, #state{socket = Socket,
           host_ip = HostIp1,
           port = Port,
           protocol = Protocol,
           path = MetricPath,
           retry_failed_metrics = RetryFailedMetrics,
	   send_timeout = SendTimeout,
           reconnect = ReconnectInterval
    }}.

exometer_report(Metric, DataPoint, Extra, Value, State) ->
 %   Key = metric_key(Metric, DataPoint),
 %   Name = name(Pfx, Metric, DataPoint),
 %   Type = counter,
 %   ok = lager:debug("event=report_metric metric=\"~p\" value=\"~p\"", [Name, Value]),
  %  Type = case exometer_util:report_type(Key, Extra, TypeMap) of
  %             {ok, T} -> T;
  %             error -> gauge
  %         end,
 %   Line = make_metric_payload(Name, Value, Type),
 %   {ok, State} = post_metric(Line, State),
    {ok, State}.

-spec exometer_subscribe(exometer_report:reporter_name(), exometer_report:metric(), exometer_report:datapoints(), exometer_report:interval(), exometer_report:extra()) ->
                       ok | not_found | unknown_reporter | error.
exometer_subscribe(_Metric, _DataPoint, _Extra, _Interval, State) ->
    {ok, State}.

-spec exometer_unsubscribe(exometer_report:metric(), 
                           exometer_report:datapoint(),
                           exometer_report:extra(), 
                           mod_state()) -> exometer_report:callback_result().
exometer_unsubscribe(_Metric, _DataPoint, _Extra, State) ->
    {ok, State}.

-spec exometer_call(any(), pid(), mod_state()) -> {ok, mod_state()}.
exometer_call(Unknown, From, State) ->
    ok = lager:warning("event=unknown_call message=\"~p\" from=\"~p\"", [Unknown, From]),
    {ok, State}.

-spec exometer_cast(any(), mod_state()) ->  {ok, mod_state()}.
exometer_cast(Unknown, State) ->
    ok = lager:warning("event=unknown_cast message=\"~p\"", [Unknown]),
    {ok, State}.

-spec exometer_info(any(),mod_state()) -> {ok, mod_state()}.
exometer_info(Unknown, State) ->
    ok = lager:warning("event=info_unsupported"),
    {ok, State}.

-spec exometer_newentry(exometer:entry(), mod_state()) -> {ok, mod_state()}.
exometer_newentry(_Entry, State) ->
    ok = lager:warning("event=new_entry_unsupported"),
    {ok, State}.

-spec exometer_setopts(exometer:entry(), exometer_report:options(),
                           exometer:status(), mod_state()) -> {ok, mod_state()}.
exometer_setopts(_Metric, _Options, _Status, State) ->
    ok = lager:warning("event=set_ops_unsupported"),
    {ok, State}.

-spec exometer_terminate(atom(), any()) -> ignore.
exometer_terminate(Reason, _) ->
    ok = lager:notice("event=terminate reason=\"~p\"",[Reason]),
    ignore.

%%% ===================================================
%%% Private API
%%% ===================================================
%% Format a graphite key from API key, prefix, prob and datapoint
%key([], [], Prob, DataPoint) ->
%    name(Prob, DataPoint);
%key([], Prefix, Prob, DataPoint) ->
%    [Prefix, $., name(Prob, DataPoint)];
%key(APIKey, [], Prob, DataPoint) ->
%    [APIKey, $., name(Prob, DataPoint)];
%key(APIKey, Prefix, Prob, DataPoint) ->
%    [APIKey, $., Prefix, $., name(Prob, DataPoint)].

%% Add probe and datapoint within probe
%name(Probe, DataPoint) ->
%    [[[metric_elem_to_list(I), $.] || I <- Probe], datapoint(DataPoint)].

%metric_elem_to_list(V) -> erlang:to_list(V).

%datapoint(V) -> erlang:to_list(V).

%% Add value, int or float, converted to list
value(V) -> erlang:to_list(V).

timestamp() ->
    integer_to_list(unix_time()).

%% @doc connect
%% Connect to graphite server to post metrics.
%% Supports only udp, tcp as backup
%% @end
connect() -> gen_udp:open(0).
connect(udp, State) ->
    % TODO
    Opts = [],
    case gen_udp:open(0, Opts) of
        {ok, Socket} ->
            {ok, #state{socket=Socket}};
        {error, Reason} ->
            {error, Reason}
    end;

connect(UnknownType, _State) ->
    {error, {UnknownType, unsupported}}.

unix_time() ->
    datetime_to_unix_time(erlang:universaltime()).

datetime_to_unix_time({{_,_,_},{_,_,_}} = DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - ?UNIX_EPOCH.


%% @doc connect
%% Construct a graphite metric payload per Carbon plaintext protocol format
%% Ref: http://graphite.readthedocs.org/en/latest/feeding-carbon.html
%% @end
%<metric path> <metric value> <metric timestamp>.
make_metric_payload(Path, Value, TimeStamp) ->
    Line = [Path, " ", value(Value), " ", TimeStamp],
    erlang:term_to_binary(Line, [compressed]).
%    Path1 = erlang:to_binary(Path),
%    Value1 = erlang:to_binary(Value),
%    TimeStamp1 = erlang:to_binary(TimeStamp),
%    Delimiter = <<" ">>,
%    <<Path1/binary, Delimiter/binary, Value1/binary, Delimiter/binary, TimeStamp1/binary>>.

post_metric([], State) ->
    {ok, State};
post_metric(Line, State) ->
    case gen_udp:send(State#state.socket, State#state.host_ip, State#state.port, Line) of
        ok ->
            {ok, State};
        {error, Reason} ->
            ok = lager:error("event=fail_to_write_metric reason=\"~p\"", [Reason]),
            State1 = connect(udp, State),
            {ok, State1}
    end.

get_opt(K, Opts) ->
    case lists:keyfind(K, 1, Opts) of
        {_, V} -> V;
        false  -> error({required, K})
    end.


get_opt(K, Opts, Default) ->
    exometer_util:get_opt(K, Opts, Default).
