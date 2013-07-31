%%%----------------------------------------------------------------------
%%% File    : echo_sup.erl
%%% Author  : Pete Kazmier <pete-trapexit@kazmier.com>
%%% Purpose : Port Tutorial
%%% Created : Fri Jan 13 12:39:27 EST 2006
%%%----------------------------------------------------------------------

-module(echo_sup).
-author('pete-trapexit@kazmier.com').

-behavior(supervisor).

%% External exports
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

start_link(ExtProg) ->
    supervisor:start_link(echo_sup, ExtProg).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

init([]) ->
    PrivDir = code:priv_dir(echo_app),
    {ok, EnvPyExtProg} = application:get_env(echo_app, pyextprog),
    {ok, EnvJavaExtProg} = application:get_env(echo_app, javaextprog),
    
    PythonExtProg = "python " ++ filename:join([PrivDir, EnvPyExtProg]),
    JavaExtProg = "java -cp " ++ PrivDir ++ " " ++ EnvJavaExtProg ,

    {ok, {{one_for_one, 3, 10},
          [
            {echopy1, {echo, start_link, [echopy1, PythonExtProg]}, permanent, 10, worker, [echo]},
            {echopy2, {echo, start_link, [echopy2, PythonExtProg]}, permanent, 10, worker, [echo]},
            {echopy3, {echo, start_link, [echopy3, PythonExtProg]}, permanent, 10, worker, [echo]},

            {echojava1, {echo, start_link, [echojava1, JavaExtProg]}, permanent, 10, worker, [echo]},
            {echojava2, {echo, start_link, [echojava2, JavaExtProg]}, permanent, 10, worker, [echo]},
            {echojava3, {echo, start_link, [echojava3, JavaExtProg]}, permanent, 10, worker, [echo]}

          ]}}.
