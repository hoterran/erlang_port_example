%%%----------------------------------------------------------------------
%%% File    : echo_app.erl
%%% Author  : Pete Kazmier <pete-trapexit@kazmier.com>
%%% Purpose : Port Tutorial
%%% Created : Fri Jan 13 12:39:27 EST 2006
%%%----------------------------------------------------------------------

-module(echo_app).
-author('pete-trapexit@kazmier.com').

-behavior(application).

%% application callbacks
-export([start/2, 
         stop/1]).

%%%----------------------------------------------------------------------
%%% Callback functions from application
%%%----------------------------------------------------------------------

start(_Type, _Args) ->
    echo_sup:start_link([]).

stop(_State) ->
    ok.
