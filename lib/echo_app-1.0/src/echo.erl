%%%----------------------------------------------------------------------
%%% File    : echo.erl
%%% Author  : Pete Kazmier <pete-trapexit@kazmier.com>
%%% Purpose : Port Tutorial
%%% Created : Fri Jan 13 12:39:27 EST 2006
%%%----------------------------------------------------------------------

-module(echo).
-author('pete-trapexit@kazmier.com').

-behavior(gen_server).

%% External exports
-export([start_link/2]).

%% API functions
-export([echopy/1, echojava/1]).

%% gen_server callbacks
-export([init/1, 
         handle_call/3,
         handle_cast/2, 
         handle_info/2,
         code_change/3,
         terminate/2]).

%% Server state
-record(state, {port}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

start_link(Name, ExtProg) ->
    gen_server:start_link({local, Name}, echo, ExtProg, []).


echojava(Msg) ->
    Id = random:uniform(3),
    gen_server:call(list_to_atom(atom_to_list(?MODULE) ++ "java" ++ integer_to_list(Id)), {echo, Msg}, get_timeout()).

echopy(Msg) ->
    ValidMsg1 = case is_newline_terminated(Msg) of
                    true  -> Msg;
                    false -> erlang:error(badarg)
                end,
    ValidMsg2 = case count_chars(ValidMsg1, $\n) of
                    1     -> ValidMsg1;
                    _     -> erlang:error(badarg)
                end,

    Id = random:uniform(3),
    gen_server:call(list_to_atom(atom_to_list(?MODULE) ++ "py" ++ integer_to_list(Id)), {echo, ValidMsg2}, get_timeout()).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

init(ExtProg) ->
    process_flag(trap_exit, true),
    %Port = open_port({spawn, ExtProg}, [stream, {line, get_maxline()}]),
    Port = open_port({spawn, ExtProg}, [stream, {line, get_maxline()}]),
    {ok, #state{port = Port}}.

handle_call({echo, Msg}, _From, #state{port = Port} = State) ->
    port_command(Port, Msg),
    case collect_response(Port) of
        {response, Response} -> 
            {reply, Response ++ erlang:process_info(self(), registered_name), State};
        timeout -> 
            {stop, port_timeout, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    {stop, {port_terminated, Reason}, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate({port_terminated, _Reason}, _State) ->
    ok;
terminate(_Reason, #state{port = Port} = _State) ->
    port_close(Port).

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

get_timeout() ->
    {ok, Value} = application:get_env(echo_app, timeout),
    Value.

get_maxline() ->
    {ok, Value} = application:get_env(echo_app, maxline),
    Value.

collect_response(Port) ->
    collect_response(Port, [], []).

collect_response(Port, RespAcc, LineAcc) ->
    receive
        {Port, {data, {eol, "OK"}}} ->
            io:format("ok~n"),
            {response, lists:reverse(RespAcc)};

        {Port, {data, {eol, Result}}} ->
            io:format("eol ~p~n", [Result]),
            Line = lists:reverse([Result | LineAcc]),
            collect_response(Port, [Line | RespAcc], []);

        {Port, {data, {noeol, Result}}} ->
            io:format("noeol ~p~n", [Result]),
            collect_response(Port, RespAcc, [Result | LineAcc])

    %% Prevent the gen_server from hanging indefinitely in case the
    %% spawned process is taking too long processing the request.
    after get_timeout() -> 
            timeout
    end.

is_newline_terminated([])    -> false;
is_newline_terminated([$\n]) -> true;
is_newline_terminated([_|T]) -> is_newline_terminated(T).
    
count_chars(String, Char) ->
    length([X || X <- String, X == Char]).
