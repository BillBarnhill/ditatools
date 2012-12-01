-module(mg_printstream).

%% Client API Exports
-export([
	start_link/0,
	start_link/1,
	copy/2,
	emit/2,
	nl/1,
	format/3,
	close/1
	]).

%% gen_server behavior exports
-export([
	init/1, 
	handle_call/2, 
	handle_cast/2, 
	handle_info/2, 
	terminate/2, 
	code_change/3]).


-record(printstream_state, {device}).


%%% Client API

start_link() ->
	start_link(standard_io).

start_link(Device) ->
	gen_server:start_link(?MODULE, [Device], []).

copy(MsgList,Stream) ->
	lists:foreach(fun (Msg) -> Stream ! Msg end, MsgList).

emit(Text,Stream) ->
	Stream ! {stream, Text},
	Stream.

nl(Stream) ->
	Stream ! {stream, nl},
	Stream.

format(Format, Args, Stream) ->
	Stream ! {stream, Format, Args},
	Stream.

close(Stream) ->
	Stream ! {stream, eof},
	ok.


%%% gen_server Implementation

init([Device]) ->
	{ok, #printstream_state{device=Device}}.

handle_call(Msg, State) ->
    handle_unexpected("handle_call", Msg, State).

handle_cast({stream, Format, Args}, State) ->
    handle_formatting(Format, Args, State);
handle_cast({stream, nl}, State) ->
    handle_newline(State);
handle_cast({stream, eos}, State) ->
    handle_eos(State);
handle_cast({stream, Text},State) ->
    handle_text(Text, State).


handle_info(Msg, State) ->
    handle_unexpected("handle_info", Msg, State).

terminate(normal, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    % none planned, here for behavior
    {ok, State}.


%%% gen_printstream (eventually) Implementation

%% Write received text to the stream
handle_text(Text,State=#printstream_state{device=Device}) ->
    out(lists:flatten(Text), Device),
    {noreply, State}.

%% Format text according to given format string, write result to stream
handle_formatting(Format, Args, State=#printstream_state{device=Device}) ->
    out(lists:flatten(io_lib:format(Format, Args)), Device),
    {noreply, State}.

%% Write newline to stream
handle_newline(State=#printstream_state{device=Device}) ->
    out(nl, Device),
    {noreply, State}.

%% Signal end of stream
handle_eos(State) ->
    {noreply, State}.

%% Handle anything else
handle_unexpected(Entry, Msg, State) ->
    io:format("Unexpected message via ~s:~n~p~n", [Msg, Entry]),
    {noreply, State}.

%% Write text to device
out(nl, standard_io) ->
    io:nl();
out(nl, Device) ->
    io:nl(Device);
out(Text, standard_io) ->
    io:put_chars(Text);
out(Text, Device) ->
    io:put_chars(Device, Text).

